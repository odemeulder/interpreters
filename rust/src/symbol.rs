//--------------------------------------------------------------------
//               S Y M B O L S
//--------------------------------------------------------------------

use crate::node_visitor::AstNode;
use std::fmt;

#[derive(Clone, Debug, Copy)]
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
  pub name: &'static str,
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

use std::rc::Rc;

#[derive(Clone)]
pub struct ProcSymbol {
  name: &'static str,
  pub params: Vec<VarSymbol>,
  pub block_ast: Rc<dyn AstNode>, // This can be removed todo
}

impl ProcSymbol {
  pub fn new(s: &'static str, params: Vec<VarSymbol>, block: Rc<dyn AstNode>) -> ProcSymbol {
    ProcSymbol {
      name: s,
      params,
      block_ast: block
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

#[derive(Clone)]
pub enum Symbol {
  None,
  Builtin(BuiltinTypeSymbol),
  Var(VarSymbol),
  Proc(ProcSymbol)
}
