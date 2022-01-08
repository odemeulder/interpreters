use std::collections::HashMap;
use crate::symbol::Symbol;
use crate::symbol::BuiltinTypeSymbol;

pub struct ScopesStack {
  scopes: Vec<Scope>
}

impl ScopesStack {

  pub fn new() -> Self {
    let mut scopes: Vec<Scope> = Vec::new();
    let new_scope = Scope::new(0, "NullScope");
    scopes.push(new_scope);
    return ScopesStack{ scopes }
  }

  pub fn push_scope(&mut self, name: &'static str) {
    let level = self.scopes[self.scopes.len() -1 ].level + 1;
    let new_scope = Scope::new(level, name);
    self.scopes.push(new_scope);
  }

  pub fn pop_scope(&mut self) {
    self.display();
    self.scopes.pop();
  }

  pub fn insert(&mut self, s: &'static str, symbol: Symbol) {
    let len = &self.scopes.len();
    if *len > 0 {
      self.scopes[len - 1].insert(s, symbol);
    }
  }

  pub fn retrieve(&mut self, s: &'static str, current_scope_only: bool) -> Symbol {
    let len = &self.scopes.len();
    for i in (0..*len).rev() {
      match &self.scopes[i].lookup(s) {
        Symbol::None => (),
        symbol => return symbol.clone(),
      }
      if current_scope_only {
        break;
      }
    }
    return Symbol::None;
  }

  pub fn display(&mut self) {
    for i in 0..self.scopes.len() {
      &self.scopes[i].display();
    }
  }

}

struct Scope {
  symbol_table: HashMap<&'static str, Symbol>,
  pub level: i32,
  pub name: &'static str,
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