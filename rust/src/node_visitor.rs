//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------
use std::rc::Rc;

use crate::lexer::TokenValue;
use crate::lexer::TokenType;
use crate::parser::*;
use crate::symbol::Symbol;
use crate::symbol::VarSymbol;
use crate::symbol::ProcSymbol;
use crate::scope::ScopesStack;
use crate::call_stack::CallStack;
use crate::call_stack::StackFrame;
use crate::call_stack::StackFrameType;
use crate::datum::Datum;
use crate::datum::VariableDatum;
use crate::datum::TypeDefDatum;
use crate::datum::ProcedureDatum;
use std::fmt;

pub trait AstNode: fmt::Display {
  fn visit(&self, _: &mut CallStack) -> Datum { Datum::None } 
  fn visit_for_sem_analysis(&self, _: &mut ScopesStack) -> () {}
  fn to_var_symbol(&self, _: &mut ScopesStack) -> Result<VarSymbol, &'static str> { Err("To_var_symbol: Undefined operation") }
  fn to_var_datum(&self) -> Result<VariableDatum, &'static str> { Err("to_var_datum: Undefined operation") }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
}

impl AstNode for Program {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Program {}", self);
    let new_frame = StackFrame::new("Global", 0, StackFrameType::Program);
    stack.push(new_frame);
    let ret_val = self.block.visit(stack);
    stack.display();
    stack.pop();
    return ret_val;
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) {
    symbols.push_scope("global");
    self.block.visit_for_sem_analysis(symbols);
    symbols.pop_scope();
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Program, name {}", self.name) }
}

impl AstNode for Block {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Block {}", self);
    for declaration in &self.declarations {
      declaration.visit(stack);
    }
    self.compound_statement.visit(stack)
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    for decl in &self.declarations {
      decl.visit_for_sem_analysis(symbols);
    }
    self.compound_statement.visit_for_sem_analysis(symbols)
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Block") }
}

impl AstNode for Compound {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Compound {}", self);
    for statement in &self.children {
      statement.visit(stack);
    }
    return Datum::None;
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    for statement in &self.children {
      statement.visit_for_sem_analysis(symbols);
    }    
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Compound") }
}

impl AstNode for ProcedureDecl {

  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Procedure decl {}", self);
    let proc_name = self.name;
    let mut param_symbols: Vec<VariableDatum> = Vec::new();
    for param in &self.params {
      if let Ok(param_var_symbol) = param.to_var_datum() {
        param_symbols.push(param_var_symbol);
      } 
    }
    let proc_block = Rc::clone(&self.block_ref);
    let procedure_datum = ProcedureDatum::new(proc_name, param_symbols, proc_block);
    stack.insert(proc_name, Datum::Procedure(procedure_datum));
    return Datum::None;
  }
  
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    let proc_name = self.name;
    let mut param_symbols: Vec<VarSymbol> = Vec::new();
    for param in &self.params {
      if let Ok(param_var_symbol) = param.to_var_symbol(symbols) {
        param_symbols.push(param_var_symbol);
      } 
    }
    let proc_symbol = ProcSymbol::new(proc_name, param_symbols);
    let symbol = Symbol::Proc(proc_symbol);
    symbols.insert(proc_name, symbol);
    symbols.push_scope(proc_name);

    for param in &self.params {
      param.visit_for_sem_analysis(symbols);
    }
    self.block_ref.visit_for_sem_analysis(symbols);
    symbols.pop_scope();
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Procedure Declaration, name {}", self.name) }
}

impl AstNode for BinOp {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit BinOp");
    let left = self.left.visit(stack);
    let right = self.right.visit(stack);
    return match (left, right) {
      (Datum::Int(l), Datum::Int(r)) => match self.token.token_type {
                              TokenType::Plus  => Datum::Int(l+r),
                              TokenType::Minus => Datum::Int(l-r),
                              TokenType::Mul   => Datum::Int(l*r),
                              TokenType::IntegerDiv   => Datum::Int(l/r), 
                              TokenType::FloatDiv   => Datum::Float(l as f64 / r as f64), 
                              _ => panic!("Unexpected binary operator"),
                            },
      (Datum::Float(l), Datum::Float(r)) => match self.token.token_type {
                              TokenType::Plus  => Datum::Float(l+r),
                              TokenType::Minus => Datum::Float(l-r),
                              TokenType::Mul   => Datum::Float(l*r),
                              TokenType::FloatDiv   => Datum::Float(l/r), 
                              _ => panic!("Unexpected binary operator"),
                            },
      (l, r) => panic!("Unexpected token left: {:#?}, right: {:#?}, token_type: {:#?}", l, r, self.token.token_type)
    }

  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) {
    self.left.visit_for_sem_analysis(symbols);
    self.right.visit_for_sem_analysis(symbols);
  }

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Binary Operation (BinOp), token_type: {}", self.token.token_type) 
  }

}

impl AstNode for Num {
  fn visit(&self, _: &mut CallStack,) -> Datum {
    println!("Visit Num");
    return match self.value {
      TokenValue::None => panic!("Interpreter Error: Unexpected Token Value type for Num"),
      TokenValue::Int(i) => Datum::Int(i),
      TokenValue::Float(f) => Datum::Float(f),
      _ => panic!("Interpreter Error: Unexpected Token Value type for Num")
    }
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Num: {}", self.value) 
  }
}

impl AstNode for Strink { 
  fn visit(&self, _: &mut CallStack,) -> Datum {
    println!("Visit Strink");
    return match self.value {
      TokenValue::String(s) => Datum::String(s),
      _ => panic!("Interpreter Error: Unexpected Token Value type for Strink")
    }
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Strink: {}", self.value) 
  }
}

impl AstNode for Boolean { 
  fn visit(&self, _: &mut CallStack,) -> Datum {
    println!("Visit Boolean");
    return match self.value {
      TokenValue::Bool(true) => Datum::Bool(true),
      TokenValue::Bool(false) => Datum::Bool(false),
      _ => panic!("Interpreter Error: Unexpected Token Value type for Boolean")
    }
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Strink: {}", self.value) 
  }
}

impl AstNode for UnaryOp {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit UnaryOp");
    match self.expr.visit(stack) {
      Datum::None => Datum::None,
      Datum::Int(i) => match self.token.token_type {
        TokenType::Plus  => Datum::Int(i),
        TokenType::Minus => Datum::Int(-i),
        _ => panic!("Invalid unary operator")
      },
      Datum::Float(f) => match self.token.token_type {
          TokenType::Plus  => Datum::Float(f),
          TokenType::Minus => Datum::Float(-f),
          _ => panic!("Invalid unary operator")
        },
      _ => panic!("Invalid unary operator")
    }
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) {
    self.expr.visit_for_sem_analysis(symbols);
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Unary Operation (UnOp), token_type: {}", self.token.token_type) 
  }

}

impl AstNode for Assign {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Assign");
    let var_name = &self.left;
    let right = self.right.visit(stack);
    stack.insert(var_name, right.clone());
    return Datum::None;
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    self.right.visit_for_sem_analysis(symbols);
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Assign, var_name: {}", self.left) 
  }
}

impl AstNode for Var {
  fn visit(&self, stack: &mut CallStack) -> Datum {
    println!("Visit Var");
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    stack.get(var_name)
  }
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = symbols.retrieve(var_name, false);
    if let Symbol::None = var_symbol {
      panic!("Error: Symbol(identifier) not found {}", var_name)
    }
  } 
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Var, var_name: {}", self.value) 
  }

}

impl AstNode for VarDecl {

  fn to_var_symbol(&self, symbols: &mut ScopesStack) -> Result<VarSymbol, &'static str> {
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid type name")
    };
    let type_symbol = match symbols.retrieve(type_name, false) {
      Symbol::Builtin(b) => *b,
      _ => return Err("Cannot declare variable, invalid built in type")
    };
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid var name")
    };
    return Ok(VarSymbol::new(var_name, type_symbol));
  }

  fn to_var_datum(&self) -> Result<VariableDatum, &'static str> {
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid type name")
    };
    let type_def = TypeDefDatum::new(type_name);
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid var name")
    };
    return Ok(VariableDatum::new(var_name, type_def));
  }

  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    let var_symbol = match self.to_var_symbol(symbols) {
      Ok(s) => s,
      Err(_) => { return; } // return early
    };
    let var_name = var_symbol.name;
    let var_symbol_lookup = symbols.retrieve(var_name, true);
    match var_symbol_lookup {
      Symbol::None => (),
      _ => panic!("Attempt to declare duplicate variable")
    }
    // insert into symbol table
    symbols.insert(var_name, Symbol::Var(var_symbol));
  }

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: VarDecl, var_name: {}", self.var_node.value ) 
  }

}

impl AstNode for Param {

  fn to_var_symbol(&self, symbols: &mut ScopesStack) -> Result<VarSymbol, &'static str> {
    let param_type_name = match (&self.type_node.token.token_type, &self.type_node.token.value) {
      (TokenType::Integer|TokenType::Real, TokenValue::String(s)) => s,
      _ => panic!("Unexpected token.")
    };
    let param_type = match symbols.retrieve(param_type_name, false) {
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

  fn to_var_datum(&self) -> Result<VariableDatum, &'static str> {
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid type name")
    };
    let type_def = TypeDefDatum::new(type_name);
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => return Err("Cannot declare variable, invalid var name")
    };
    return Ok(VariableDatum::new(var_name, type_def));
  }

  
  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    let var_symbol = self.to_var_symbol(symbols).unwrap();
    let param_name = var_symbol.name;
    symbols.insert(param_name, Symbol::Var(var_symbol));
  }

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Param, param: {}", self.var_node.value) 
  }

}

impl AstNode for ProcCall {
  
  fn visit(&self, stack: &mut CallStack) -> Datum { 
    println!("Visit ProcCall");
    let proc_name = self.proc_name;
    let curr_level = match stack.peek() {
      None => 0,
      Some(frame) => frame.level
    };
    let new_frame = StackFrame::new(proc_name, curr_level + 1, StackFrameType::Procedure);
    stack.push(new_frame);

    // Retrieve procedure declaration from call stack
    let procedure_datum: ProcedureDatum = match stack.get(proc_name) {
      Datum::Procedure(proc) => proc,
      _ => panic!("There should be a procedure datum defined for {}", proc_name)
    };
    // match arguments with parameters
    let args = &self.args;
    let params = procedure_datum.params;
    let mut iter = params.iter().zip(args);
    loop {
      match iter.next() {
        Some((var_datum, var_node)) => {
          let var_content = var_node.visit(stack);
          stack.insert(var_datum.name, var_content)
        },
        None => break
      }
    }
    stack.display();

    // execute the call here
    procedure_datum.block_ast.visit(stack);

    stack.pop();
    
    Datum::None
  }


  fn visit_for_sem_analysis(&self, symbols: &mut ScopesStack) { 
    for arg in &self.args {
      arg.visit_for_sem_analysis(symbols);
    }
    // code to verify that number of args = number of params .
    let proc_name = self.proc_name;
    let proc_symbol = match symbols.retrieve(proc_name, false) {
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

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Procedure Call, proc_name: {}", self.proc_name) 
  }

}

impl AstNode for WriteStatement {
  fn visit(&self, _: &mut CallStack) -> Datum { 
    println!("{}", self.content);
    if self.new_line { println!("") }
    Datum::None
  }
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: WriteStatement  {}", self.content) 
  }
}

impl AstNode for Type { 
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: Type, type: {}", self.value) 
  }
}

impl AstNode for NoOp { 

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
    write!(f, "AST Node: NoOP") 
  }

}
