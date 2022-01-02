//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

use crate::lexer::TokenValue;
use crate::lexer::TokenType;
use crate::parser::*;
use crate::symbol_table;
use crate::symbol_table::Symbol;
use crate::symbol_table::VarSymbol;
use crate::global_scope;

pub trait AstNode {
  fn visit(&self) -> TokenValue;
  fn visit_for_symbols(&self);
  fn visit_for_sem_analysis(&self);
}

impl AstNode for BinOp {
  fn visit(&self) -> TokenValue {
    let left = self.left.visit();
    let right = self.right.visit();
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
  fn visit_for_symbols(&self) { 
    self.left.visit_for_symbols();
    self.right.visit_for_symbols();
  }
  fn visit_for_sem_analysis(&self) {
    self.left.visit_for_sem_analysis();
    self.right.visit_for_sem_analysis();
  }
}

impl AstNode for Num {
  fn visit(&self) -> TokenValue {
    return match self.value {
      TokenValue::None => panic!("Invalid node"),
      TokenValue::Int(i) => TokenValue::Int(i),
      TokenValue::Float(f) => TokenValue::Float(f),
      _ => panic!("Invalid node")
    }
  }
  fn visit_for_symbols(&self) { }
  fn visit_for_sem_analysis(&self) { }
}

impl AstNode for UnaryOp {
  fn visit(&self) -> TokenValue {
    match self.expr.visit() {
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
  fn visit_for_symbols(&self) {
    self.expr.visit_for_symbols();
  }
  fn visit_for_sem_analysis(&self) {
    self.expr.visit_for_sem_analysis();
  }
}

impl AstNode for Compound {
  fn visit(&self) -> TokenValue {
    for statement in &self.children {
      statement.visit();
    }
    return TokenValue::None;
  }
  fn visit_for_symbols(&self) { 
    for statement in &self.children {
      statement.visit_for_symbols();
    }    
  }
  fn visit_for_sem_analysis(&self) { 
    for statement in &self.children {
      statement.visit_for_sem_analysis();
    }    
  }
}

impl AstNode for Assign {
  fn visit(&self) -> TokenValue {
    let var_name = &self.left;
    let right = self.right.visit();
    // let mut guard = GLOBAL_SCOPE.lock().unwrap();
    // guard.insert(var_name.to_string(), right);
    global_scope::insert(var_name, right);
    TokenValue::None 
  }
  fn visit_for_symbols(&self) { 
    let var_name = self.left;
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Assign to undeclared variable name: {}", var_name)
    }
    self.right.visit_for_symbols()
  }
  fn visit_for_sem_analysis(&self) { 
    let var_name = self.left;
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Assign to undeclared variable name: {}", var_name)
    }
    self.right.visit_for_symbols()
   }
}

impl AstNode for Var {
  fn visit(&self) -> TokenValue {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    return global_scope::retrieve(var_name);
  }
  fn visit_for_symbols(&self) { 
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Unknown variable name: {}", var_name)
    }
  }
  fn visit_for_sem_analysis(&self) {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Error: Symbol(identifier) not found {}", var_name)
    }
  }
}

impl AstNode for Block {
  fn visit(&self) -> TokenValue {
    self.compound_statement.visit()
  }
  fn visit_for_symbols(&self) {
    for decl in &self.declarations {
      decl.visit_for_symbols();
    } 
    self.compound_statement.visit_for_symbols()
  }
  fn visit_for_sem_analysis(&self) { 
    for decl in &self.declarations {
      decl.visit_for_sem_analysis();
    }
    self.compound_statement.visit_for_sem_analysis()
 }
}

impl AstNode for VarDecl {
  fn visit(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_for_symbols(&self) { 
  }
  fn visit_for_sem_analysis(&self) { 
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid type name")
    };
    let type_symbol = match symbol_table::lookup_symbol(type_name) {
      symbol_table::Symbol::Builtin(b) => b,
      s => { panic!("Cannot declare variable, invalid built in {:#?}", s); }
    };
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid var name")
    };
    let var_symbol = VarSymbol::new(var_name, type_symbol);
    // prevent addition of duplicate symbols
    let var_symbol_lookup = symbol_table::lookup_symbol(var_name);
    match var_symbol_lookup {
      Symbol::None => (),
      _ => panic!("Attempt to declare duplicate variable")
    }
    // insert into symbol table
    symbol_table::define_symbol(Symbol::Var(var_symbol));
  }
}

impl AstNode for Type {
  fn visit(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_for_symbols(&self) { }
  fn visit_for_sem_analysis(&self) { }
}

impl AstNode for Program {
  fn visit(&self) -> TokenValue {
    return self.block.visit();
  }
  fn visit_for_symbols(&self) { 
    self.block.visit_for_symbols()
  }
  fn visit_for_sem_analysis(&self) {
    self.block.visit_for_sem_analysis()
  }
}

impl AstNode for ProcedureDecl {
  fn visit(&self) -> TokenValue {
    return TokenValue::None;
  }
  fn visit_for_symbols(&self) { 
    ()
  }
  fn visit_for_sem_analysis(&self) { }
}

impl AstNode for NoOp {
  fn visit(&self) -> TokenValue {
    TokenValue::None 
  }
  fn visit_for_symbols(&self) { }
  fn visit_for_sem_analysis(&self) { }
}
