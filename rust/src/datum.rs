//--------------------------------------------------------------------
//               D A T U M 
//--------------------------------------------------------------------
// Stuff that gets stored in a variable or returned from a program

use std::rc::Rc;
use std::fmt;
use crate::node_visitor::AstNode;


#[derive(Clone, Debug)]
pub enum Datum {
  None,
  String(&'static str),
  Int(i32),
  Float(f64),
  Procedure(ProcedureDatum),
  Var(VariableDatum),
  Type(TypeDefDatum)
}

impl Datum {
  pub fn new_type(name: &'static str) -> Self {
    Datum::Type(TypeDefDatum { name })
  }
  pub fn new_variable(name: &'static str, type_def: TypeDefDatum) -> Self {
    Datum::Var(VariableDatum { name, type_def })
  }
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
  name: &'static str,
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
