//--------------------------------------------------------------------
//               S Y M B O L S
//--------------------------------------------------------------------

use std::fmt;
use std::collections::HashMap;
use std::sync::Mutex;
use crate::parser::Parser;

#[derive(Clone, Debug)]
pub struct BuiltinTypeSymbol {
  name: &'static str,
}

impl BuiltinTypeSymbol {
  fn new(s: &'static str) -> BuiltinTypeSymbol {
    BuiltinTypeSymbol {
      name: s,
    }
  }
}

#[derive(Clone, Debug)]
pub struct VarSymbol {
  name: &'static str,
  symbol_type: BuiltinTypeSymbol,
}
impl VarSymbol {
  pub fn new(s: &'static str, tt:BuiltinTypeSymbol) -> VarSymbol {
    VarSymbol {
      name: s,
      symbol_type: tt
    }
  }
}

#[derive(Clone, Debug)]
struct ProcSymbol {
  name: &'static str,
}
impl ProcSymbol {
  fn new(s: &'static str) -> ProcSymbol {
    ProcSymbol {
      name: s
    }
  }
}

trait NamedSymbol {
  fn get_name(&mut self) -> &'static str;
}

impl NamedSymbol for Symbol {
  fn get_name(&mut self) -> &'static str {
    match &self {
      Symbol::Builtin(b) => b.name,
      Symbol::Var(v) => v.name,
      Symbol::Proc(p) => p.name,
      Symbol::None => "NONE"
    }
  }
}

impl fmt::Display for BuiltinTypeSymbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self.name)
  }
}
impl fmt::Display for VarSymbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "<{}:{}>", self.name, self.symbol_type)
  }
}

#[derive(Clone, Debug)]
pub enum Symbol {
  None,
  Builtin(BuiltinTypeSymbol),
  Var(VarSymbol),
  Proc(ProcSymbol)
}

lazy_static! {
  static ref SYMBOL_TABLE: Mutex<HashMap<&'static str, Symbol>> = Mutex::new(HashMap::new());
}

pub fn define_symbol(sym: Symbol) -> () {
  let mut map = SYMBOL_TABLE.lock().unwrap();
  let n = sym.clone().get_name();
  map.insert(n, sym);
}

fn init_symbol_table() -> () {
  define_symbol(Symbol::Builtin(BuiltinTypeSymbol::new("INTEGER")));
  define_symbol(Symbol::Builtin(BuiltinTypeSymbol::new("REAL")));
}

pub fn lookup_symbol(s: &'static str) -> Symbol {
  let map = SYMBOL_TABLE.lock().unwrap();
  match map.get(s) {
    None => Symbol::None,
    Some(s) => s.clone() 
  }
}

pub fn display() {
  println!("Symbol Table {:?}", SYMBOL_TABLE.lock().unwrap());
}

pub struct SymbolTableBuilder {
  parser: Parser
}
impl SymbolTableBuilder {
  pub fn new(parser: Parser) -> SymbolTableBuilder {
    init_symbol_table();
    return SymbolTableBuilder {
      parser
    }
  }
  pub fn build(&mut self) -> () {
    let tree = self.parser.parse();
    tree.visit_node_for_symbols();
  }
}

