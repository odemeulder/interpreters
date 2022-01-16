//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

use crate::parser::Parser;
use crate::datum::Datum;
use crate::call_stack::CallStack;

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
  pub fn interpret(&mut self) -> Datum {
    let tree = &self.parser.parse();
    let mut call_stack = CallStack::new();
    let symbols = crate::scope::ScopesStack::new();
    let rv = tree.visit(&mut call_stack, &symbols);
    call_stack.display();
    return rv;
  }
}
