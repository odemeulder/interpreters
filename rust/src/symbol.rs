//--------------------------------------------------------------------
//               S Y M B O L S
//--------------------------------------------------------------------

use std::fmt;

#[derive(Clone, Debug)]
pub struct BuiltinTypeSymbol {
  name: &'static str,
}

impl BuiltinTypeSymbol {
  pub fn new(s: &'static str) -> BuiltinTypeSymbol {
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
pub struct ProcSymbol {
  name: &'static str,
}
impl ProcSymbol {
  pub fn new(s: &'static str) -> ProcSymbol {
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
