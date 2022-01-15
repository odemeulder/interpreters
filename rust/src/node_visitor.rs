//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

use crate::lexer::TokenValue;
use crate::lexer::TokenType;
use crate::parser::*;
use crate::symbol::Symbol;
use crate::symbol::VarSymbol;
use crate::symbol::ProcSymbol;
use crate::scope;
use crate::call_stack::CallStack;
use crate::call_stack::StackFrame;
use crate::call_stack::StackFrameType;

pub trait AstNode {
  fn visit(&self, stack: &mut CallStack) -> TokenValue;
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) -> () {}
  fn to_var_symbol(&self, _: &mut scope::ScopesStack) -> Result<VarSymbol, &'static str> { Err("To_var_symbol: Undefined operation") }
}

impl AstNode for Program {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    let new_frame = StackFrame::new("Global", 0, StackFrameType::Program);
    stack.push(new_frame);
    let ret_val = self.block.visit(stack);
    stack.display();
    stack.pop();
    return ret_val;
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) {
    scope_stack.push_scope("global");
    self.block.visit_for_sem_analysis(scope_stack);
    scope_stack.pop_scope();
  }
}

impl AstNode for Block {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    self.compound_statement.visit(stack)
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    for decl in &mut self.declarations {
      decl.visit_for_sem_analysis(scope_stack);
    }
    self.compound_statement.visit_for_sem_analysis(scope_stack)
  }
}

impl AstNode for Compound {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    for statement in &self.children {
      statement.visit(stack);
    }
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    for statement in &mut self.children {
      statement.visit_for_sem_analysis(scope_stack);
    }    
  }
}

use std::rc::Rc;

impl AstNode for ProcedureDecl {
  fn visit(&self, _: &mut CallStack) -> TokenValue {
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    let proc_name = self.name;
    let mut param_symbols: Vec<VarSymbol> = Vec::new();
    for param in &self.params {
      if let Ok(param_var_symbol) = param.to_var_symbol(scope_stack) {
        param_symbols.push(param_var_symbol);
      } 
    }
    let proc_block = Rc::clone(&self.block_ref);
    let proc_symbol = ProcSymbol::new(proc_name, param_symbols, proc_block);
    let symbol = Symbol::Proc(proc_symbol);
    scope_stack.insert(proc_name, symbol);
    scope_stack.push_scope(proc_name);

    for param in &mut self.params {
      param.visit_for_sem_analysis(scope_stack);
    }
    self.block.visit_for_sem_analysis(scope_stack);
    scope_stack.pop_scope();
  }
}

impl AstNode for BinOp {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    let left = self.left.visit(stack);
    let right = self.right.visit(stack);
    return match (left, right) {
      (TokenValue::Int(l), TokenValue::Int(r)) => match self.token.token_type {
                              TokenType::Plus  => TokenValue::Int(l+r),
                              TokenType::Minus => TokenValue::Int(l-r),
                              TokenType::Mul   => TokenValue::Int(l*r),
                              TokenType::IntegerDiv   => TokenValue::Int(l/r), 
                              TokenType::FloatDiv   => TokenValue::Float(l as f64 / r as f64), 
                              _ => panic!("Unexpected binary operator"),
                            },
      (TokenValue::Float(l), TokenValue::Float(r)) => match self.token.token_type {
                              TokenType::Plus  => TokenValue::Float(l+r),
                              TokenType::Minus => TokenValue::Float(l-r),
                              TokenType::Mul   => TokenValue::Float(l*r),
                              TokenType::FloatDiv   => TokenValue::Float(l/r), 
                              _ => panic!("Unexpected binary operator"),
                            },
      (l, r) => panic!("Unexpected token left: {:#?}, right: {:#?}, token_type: {:#?}", l, r, self.token.token_type)
    }

  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) {
    self.left.visit_for_sem_analysis(scope_stack);
    self.right.visit_for_sem_analysis(scope_stack);
  }
}

impl AstNode for Num {
  fn visit(&self, _: &mut CallStack) -> TokenValue {
    return match self.value {
      TokenValue::None => panic!("Invalid node"),
      TokenValue::Int(i) => TokenValue::Int(i),
      TokenValue::Float(f) => TokenValue::Float(f),
      _ => panic!("Invalid node")
    }
  }
}

impl AstNode for UnaryOp {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    match self.expr.visit(stack) {
      TokenValue::None => TokenValue::None,
      TokenValue::Int(i) => match self.token.token_type {
        TokenType::Plus  => TokenValue::Int(i),
        TokenType::Minus => TokenValue::Int(-i),
        _ => panic!("Invalid unary operator")
      },
      TokenValue::Float(f) => match self.token.token_type {
          TokenType::Plus  => TokenValue::Float(f),
          TokenType::Minus => TokenValue::Float(-f),
          _ => panic!("Invalid unary operator")
        },
      _ => panic!("Invalid unary operator")
    }
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) {
    self.expr.visit_for_sem_analysis(scope_stack);
  }
}

impl AstNode for Assign {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    let var_name = &self.left;
    let right = self.right.visit(stack);
    stack.insert(var_name, right.clone());
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    self.right.visit_for_sem_analysis(scope_stack);
  }
}

impl AstNode for Var {
  fn visit(&self, stack: &mut CallStack) -> TokenValue {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    stack.get(var_name)
    // let retval = global_memory::get(var_name);
    // return retval;
  }
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = scope_stack.retrieve(var_name, false);
    if let Symbol::None = var_symbol {
      panic!("Error: Symbol(identifier) not found {}", var_name)
    }
  }
}


impl AstNode for VarDecl {

  fn visit(&self, _: &mut CallStack) -> TokenValue {
    TokenValue::None  
  }

  fn to_var_symbol(&self, scope_stack: &mut scope::ScopesStack) -> Result<VarSymbol, &'static str> {
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid type name")
    };
    let type_symbol = match scope_stack.retrieve(type_name, false) {
      Symbol::Builtin(b) => *b,
      _ => return Err("Cannot declare variable, invalid built in type")
    };
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid var name")
    };
    return Ok(VarSymbol::new(var_name, type_symbol));
  }

  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    let var_symbol = match self.to_var_symbol(scope_stack) {
      Ok(s) => s,
      Err(_) => { return; } // return early
    };
    let var_name = var_symbol.name;
    let var_symbol_lookup = scope_stack.retrieve(var_name, true);
    match var_symbol_lookup {
      Symbol::None => (),
      _ => panic!("Attempt to declare duplicate variable")
    }
    // insert into symbol table
    scope_stack.insert(var_name, Symbol::Var(var_symbol));
  }
}

impl AstNode for Param {

  fn visit(&self, _: &mut CallStack) -> TokenValue { 
    return TokenValue::None; // to do
  }

  fn to_var_symbol(&self, scope_stack: &mut scope::ScopesStack) -> Result<VarSymbol, &'static str> {
    let param_type_name = match (&self.type_node.token.token_type, &self.type_node.token.value) {
      (TokenType::Integer|TokenType::Real, TokenValue::String(s)) => s,
      _ => panic!("Unexpected token.")
    };
    let param_type = match scope_stack.retrieve(param_type_name, false) {
      Symbol::Builtin(b) => *b,
      _ => panic!("TODO PANIC") 
    };
    let param_name = match (&self.type_node.token.token_type, &self.var_node.value) {
      (TokenType::Integer|TokenType::Real, TokenValue::String(s)) => s,
      _ => panic!("Unexpected token.")
    };
    let var_symbol = VarSymbol::new(param_name, param_type);
    return Ok(var_symbol);
  }
  
  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    let var_symbol = self.to_var_symbol(scope_stack).unwrap();
    let param_name = var_symbol.name;
    scope_stack.insert(param_name, Symbol::Var(var_symbol));
  }
}

impl AstNode for ProcCall {
  
  fn visit(&self, stack: &mut CallStack) -> TokenValue { 
    let proc_name = self.proc_name;
    let new_frame = StackFrame::new(proc_name, 2, StackFrameType::Procedure);
    stack.push(new_frame);
    let args = &self.args;
    let params = match &self.proc_symbol{
      Some(p) => &p.params,
      None => panic!("Params expected to be defined for proc symbol")
    };
    let mut iter = params.iter().zip(args);
    loop {
      match iter.next() {
        Some((var_symbol, var_node)) => {
          let var_content = var_node.visit(stack);
          stack.insert(var_symbol.name, var_content)
        },
        None => break
      }
    }

    stack.display();
    stack.pop();
    
    TokenValue::None
  }


  fn visit_for_sem_analysis(&mut self, scope_stack: &mut scope::ScopesStack) { 
    for arg in &mut self.args {
      arg.visit_for_sem_analysis(scope_stack);
    }
    // code to verify that number of args = number of params .
    let proc_name = self.proc_name;
    let proc_symbol = match scope_stack.retrieve(proc_name, false) {
      Symbol::Proc(proc) => proc,
      _ => panic!("There should be a proc symbol defined for {}", proc_name)
    };
    let num_args = self.args.len();
    let num_params = proc_symbol.params.len();
    if num_args != num_params {
      panic!("Incorrect number of arguments for procedure call, expected {}, got {}", num_params, num_args);
    }

    // assign proc symbol to ProcCall object /// LET'S NOT DO THAT /// TODO 
    //self.proc_symbol = Some(proc_symbol);
  }
}

impl AstNode for Type {
  fn visit(&self, _: &mut CallStack) -> TokenValue {
    TokenValue::None  
  }
}


impl AstNode for NoOp {
  fn visit(&self, _: &mut CallStack) -> TokenValue {
    TokenValue::None 
  }
}
