//--------------------------------------------------------------------
//               D A T U M 
//--------------------------------------------------------------------
// Stuff that gets stored in a variable or returned from a program

use std::rc::Rc;
use std::fmt;
use crate::node_visitor::AstNode;
use std::io::Write;

#[derive(Clone, Debug)]
pub enum Datum {
  None,
  String(&'static str),
  Int(i32),
  Float(f64),
  Bool(bool),
  Procedure(ProcedureDatum),
  // Var(VariableDatum),
}

impl fmt::Display for Datum {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      match *self {
        Datum::None => write!(f, "Empty Datum"),
        Datum::String(s) => write!(f, "{}", s),
        Datum::Int(i) => write!(f, "{}", i),
        Datum::Float(float) => write!(f, "float {}", float),
        Datum::Bool(b) => write!(f, "{}", b),
        Datum::Procedure(_) => write!(f, "procedure datum")
      }
  }
}

impl Datum {
  // pub fn new_type(name: &'static str) -> Self {
  //   Datum::Type(TypeDefDatum { name })
  // }
  // pub fn new_variable(name: &'static str, type_def: TypeDefDatum) -> Self {
  //   Datum::Var(VariableDatum { name, type_def })
  // }
}

#[derive(Clone, Debug)]
pub struct TypeDefDatum {
  name: &'static str
}

impl TypeDefDatum {
  pub fn new(name: &'static str) -> Self { TypeDefDatum {name }}
}

#[derive(Clone, Debug)]
pub struct VariableDatum {
  pub name: &'static str,
  type_def: TypeDefDatum
}

impl VariableDatum {
  pub fn new(name: &'static str, type_def: TypeDefDatum) -> Self { VariableDatum {name, type_def }}
}


#[derive(Clone)]
pub struct ProcedureDatum {
  name: &'static str,
  pub params: Vec<VariableDatum>,
  pub block_ast: Rc<dyn AstNode>,
}
impl fmt::Display for ProcedureDatum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "<{}:ProcedureDatum> |block: {}|", self.name, self.block_ast)
  }
}
impl ProcedureDatum {
  pub fn new(name: &'static str, params: Vec<VariableDatum>, block_ast: Rc<dyn AstNode> ) -> Self {
    ProcedureDatum {
      name,
      params,
      block_ast
    }
  }
}

impl fmt::Debug for ProcedureDatum {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "<{}:Procedure>", self.name)
  }
}
