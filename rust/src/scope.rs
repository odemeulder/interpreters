use std::collections::HashMap;
use crate::symbol::Symbol;
use crate::symbol::BuiltinTypeSymbol;

pub struct Scope {
  symbol_table: HashMap<&'static str, Symbol>,
  pub level: i32,
  pub name: &'static str,
  // pub enclosing_scope: Option<Box<&'a mut Scope<'a>>>
}

impl Scope {

  pub fn new(level: i32, name: &'static str) -> Scope {
    let mut map: HashMap<&'static str, Symbol> = HashMap::new();
    map.insert("INTEGER", Symbol::Builtin(BuiltinTypeSymbol::new("INTEGER")));
    map.insert("REAL", Symbol::Builtin(BuiltinTypeSymbol::new("REAL")));
    return Scope {
      symbol_table: map,
      level,
      name,
    }
  }

  pub fn insert(&mut self, s: &'static str, symbol: Symbol) {
    &self.symbol_table.insert(s, symbol);
  }

  pub fn lookup(&mut self, s: &'static str) -> Symbol {
    match self.symbol_table.get(s) {
      None => Symbol::None,
      Some(s) => s.clone()
    }
  }

  pub fn display(&mut self) {
    println!("Scope name {}, level {}", self.name, self.level);
    println!("{:?}", &self.symbol_table)
  }
}