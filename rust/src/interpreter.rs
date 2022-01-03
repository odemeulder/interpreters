//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

use crate::parser::Parser;
use crate::lexer::TokenValue;
use crate::scope;

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
    let mut scope = scope::Scope::new(0, "initial");
    let rv = tree.visit(&mut scope);
    scope.display();
    return rv;
  }
}
