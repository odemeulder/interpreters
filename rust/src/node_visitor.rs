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
  fn visit_node(&self) -> TokenValue;
  fn visit_node_for_symbols(&self);
}

impl AstNode for BinOp {
  fn visit_node(&self) -> TokenValue {
    let left = self.left.visit_node();
    let right = self.right.visit_node();
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
  fn visit_node_for_symbols(&self) { 
    self.left.visit_node_for_symbols();
    self.right.visit_node_for_symbols();
  }
}

impl AstNode for Num {
  fn visit_node(&self) -> TokenValue {
    return match self.value {
      TokenValue::None => panic!("Invalid node"),
      TokenValue::Int(i) => TokenValue::Int(i),
      TokenValue::Float(f) => TokenValue::Float(f),
      _ => panic!("Invalid node")
    }
  }
  fn visit_node_for_symbols(&self) { }
}

impl AstNode for UnaryOp {
  fn visit_node(&self) -> TokenValue {
    match self.expr.visit_node() {
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
  fn visit_node_for_symbols(&self) {
    self.expr.visit_node_for_symbols();
  }
}

impl AstNode for Compound {
  fn visit_node(&self) -> TokenValue {
    for statement in &self.children {
      statement.visit_node();
    }
    return TokenValue::None;
  }
  fn visit_node_for_symbols(&self) { 
    for statement in &self.children {
      statement.visit_node_for_symbols();
    }    
  }
}

impl AstNode for Assign {
  fn visit_node(&self) -> TokenValue {
    let var_name = &self.left;
    let right = self.right.visit_node();
    // let mut guard = GLOBAL_SCOPE.lock().unwrap();
    // guard.insert(var_name.to_string(), right);
    global_scope::insert(var_name, right);
    TokenValue::None 
  }
  fn visit_node_for_symbols(&self) { 
    let var_name = self.left;
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Assign to undeclared variable name: {}", var_name)
    }
    self.right.visit_node_for_symbols()
  }
}

impl AstNode for Var {
  fn visit_node(&self) -> TokenValue {
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    return global_scope::retrieve(var_name);
  }
  fn visit_node_for_symbols(&self) { 
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = symbol_table::lookup_symbol(var_name);
    if let symbol_table::Symbol::None = var_symbol {
      panic!("Unknown variable name: {}", var_name)
    }
  }
}

impl AstNode for Block {
  fn visit_node(&self) -> TokenValue {
    // for decl in self.declarations {
    //   decl.visit_node();
    // } // SINCE visit VarDecl does nothing for now. leaving this out.
    self.compound_statement.visit_node()
  }
  fn visit_node_for_symbols(&self) {
    for decl in &self.declarations {
      decl.visit_node_for_symbols();
    } 
    self.compound_statement.visit_node_for_symbols()
  }
}

impl AstNode for VarDecl {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_node_for_symbols(&self) { 
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
    symbol_table::define_symbol(Symbol::Var(var_symbol));
  }
}
impl AstNode for Type {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_node_for_symbols(&self) { }
}

impl AstNode for Program {
  fn visit_node(&self) -> TokenValue {
    return self.block.visit_node();
  }
  fn visit_node_for_symbols(&self) { 
    self.block.visit_node_for_symbols()
  }
}

impl AstNode for ProcedureDecl {
  fn visit_node(&self) -> TokenValue {
    return TokenValue::None;
  }
  fn visit_node_for_symbols(&self) { 
    ()
  }
}

impl AstNode for NoOp {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None 
  }
  fn visit_node_for_symbols(&self) { }
}
