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
use crate::global_memory;

pub trait AstNode {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue;
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack);
}

impl AstNode for Program {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    return self.block.visit(scope_stack);
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) {
    scope_stack.push_scope("global");
    self.block.visit_for_sem_analysis(scope_stack);
    scope_stack.pop_scope();
  }
}

impl AstNode for Block {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    self.compound_statement.visit(scope_stack)
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
    for decl in &self.declarations {
      decl.visit_for_sem_analysis(scope_stack);
    }
    self.compound_statement.visit_for_sem_analysis(scope_stack)
  }
}

impl AstNode for Compound {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    for statement in &self.children {
      statement.visit(scope_stack);
    }
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
    for statement in &self.children {
      statement.visit_for_sem_analysis(scope_stack);
    }    
  }
}

impl AstNode for ProcedureDecl {
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
    let proc_name = self.name;
    let proc_symbol = Symbol::Proc(ProcSymbol::new(proc_name));
    scope_stack.insert(proc_name, proc_symbol);
    scope_stack.push_scope(proc_name);

    for param in &self.params {
      param.visit_for_sem_analysis(scope_stack);
    }
    self.block.visit_for_sem_analysis(scope_stack);
    scope_stack.pop_scope();
  }
}

impl AstNode for BinOp {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    let left = self.left.visit(scope_stack);
    let right = self.right.visit(scope_stack);
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
      _ => panic!("Unexpected token")
    }

  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) {
    self.left.visit_for_sem_analysis(scope_stack);
    self.right.visit_for_sem_analysis(scope_stack);
  }
}

impl AstNode for Num {
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    return match self.value {
      TokenValue::None => panic!("Invalid node"),
      TokenValue::Int(i) => TokenValue::Int(i),
      TokenValue::Float(f) => TokenValue::Float(f),
      _ => panic!("Invalid node")
    }
  }
  fn visit_for_sem_analysis(&self, _: &mut scope::ScopesStack) { }
}

impl AstNode for UnaryOp {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    match self.expr.visit(scope_stack) {
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
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) {
    self.expr.visit_for_sem_analysis(scope_stack);
  }
}

impl AstNode for Assign {
  fn visit(&self, scope_stack: &mut scope::ScopesStack) -> TokenValue {
    let var_name = &self.left;
    let right = self.right.visit(scope_stack);
    global_memory::insert(var_name, right);
    return TokenValue::None;
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
  self.right.visit_for_sem_analysis(scope_stack);
  //self.left.visit(scope_stack);
  }
}

impl AstNode for Var {
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    let retval = global_memory::get(var_name);
    return retval;
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) {
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
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    TokenValue::None  
  }
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid type name")
    };
    let type_symbol = match scope_stack.retrieve(type_name, false) {
      Symbol::Builtin(b) => b,
      s => { panic!("Cannot declare variable, invalid built in {:#?}", s); }
    };
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid var name")
    };
    let var_symbol = VarSymbol::new(var_name, type_symbol);
    // prevent addition of duplicate symbols
    // let var_symbol_lookup = symbol_table::lookup_symbol(var_name);
    let var_symbol_lookup = scope_stack.retrieve(var_name, true);
    match var_symbol_lookup {
      Symbol::None => (),
      _ => panic!("Attempt to declare duplicate variable")
    }
    // insert into symbol table
    // symbol_table::define_symbol(Symbol::Var(var_symbol));
    scope_stack.insert(var_name, Symbol::Var(var_symbol));
  }
}

impl AstNode for Param {

  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue { 
    return TokenValue::None; // to do
  }
  
  fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
    let param_type_name = match (&self.type_node.token.token_type, &self.type_node.token.value) {
      (TokenType::Integer|TokenType::Real, TokenValue::String(s)) => s,
      _ => panic!("Unexpected token.")
    };
    let param_type = match scope_stack.retrieve(param_type_name, false) {
      Symbol::Builtin(b) => b,
      _ => panic!("TODO PANIC") 
    };
    let param_name = match (&self.type_node.token.token_type, &self.var_node.value) {
      (TokenType::Integer|TokenType::Real, TokenValue::String(s)) => s,
      _ => panic!("Unexpected token.")
    };
    let var_symbol = Symbol::Var(VarSymbol::new(param_name, param_type));
    scope_stack.insert(param_name, var_symbol);
  }
}

impl AstNode for ProcCall {
  
fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue { 
  TokenValue::None
}
fn visit_for_sem_analysis(&self, scope_stack: &mut scope::ScopesStack) { 
  for arg in &self.args {
    arg.visit_for_sem_analysis(scope_stack);
  }  
}
}

impl AstNode for Type {
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    TokenValue::None  
  }
  fn visit_for_sem_analysis(&self, _: &mut scope::ScopesStack) { }
}


impl AstNode for NoOp {
  fn visit(&self, _: &mut scope::ScopesStack) -> TokenValue {
    TokenValue::None 
  }
  fn visit_for_sem_analysis(&self, _: &mut scope::ScopesStack) { }
}
