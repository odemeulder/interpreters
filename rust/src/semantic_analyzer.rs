//--------------------------------------------------------------------
//               S E M A N T I C   A N A L Y Z E R
//--------------------------------------------------------------------

use crate::parser::Parser;
use crate::symbol_table;
use crate::scope;

pub struct SemanticAnalyzer {
  pub parser: Parser
}

impl SemanticAnalyzer {
  
  pub fn new(parser: Parser) -> SemanticAnalyzer {
    symbol_table::init_symbol_table();
    SemanticAnalyzer {
      parser
    }
  }

  pub fn analyze(&mut self) {
    let node = self.parser.parse();
    let mut scope = scope::Scope::new(0, "initial");
    node.visit_for_sem_analysis(&mut scope);
  }
}

