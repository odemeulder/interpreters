//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

use crate::parser::Parser;
use crate::lexer::TokenValue;

#[derive(Debug)]
pub struct Interpreter {
  parser: Parser
}

pub fn build_interpreter(parser: Parser) -> Interpreter {
  return Interpreter {
    parser
  }
}

impl Interpreter {
  pub fn interpret(&mut self) -> TokenValue {
    let tree = &self.parser.parse();
    return tree.visit_node();
  }
}
