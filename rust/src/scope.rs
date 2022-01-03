use std::collections::HashMap;
use crate::lexer::TokenValue;

pub struct Scope {
  symbol_table: HashMap<&'static str, TokenValue>,
  level: i32,
  name: &'static str
}

impl Scope {

  pub fn new(level: i32, name: &'static str) -> Scope {
    return Scope {
      symbol_table: HashMap::new(),
      level,
      name
    }
  }

  pub fn insert(&mut self, s: &'static str, symbol: TokenValue) {
    &self.symbol_table.insert(s, symbol);
  }

  pub fn lookup(&mut self, s: &'static str) -> TokenValue {
    match self.symbol_table.get(s) {
      None => TokenValue::None,
      Some(s) => s.clone()
    }
  }

  pub fn display(&mut self) {
    println!("Scope name {}, level {}", self.name, self.level);
    println!("{:?}", &self.symbol_table)
  }
}