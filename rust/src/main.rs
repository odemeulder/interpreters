#[macro_use]
extern crate lazy_static;

use std::fs;

mod lexer;
mod symbol;
mod parser;
mod node_visitor;
mod interpreter;
mod semantic_analyzer;
mod scope;
mod global_memory;
mod call_stack;

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------


fn main() {

  println!("ODM Interpreter");
  let progr = fs::read_to_string("program18.txt").unwrap();
  println!("Program: {:#?}", &progr);
  let lexer = lexer::build_lexer(progr);
  let parser = parser::build_parser(lexer);

  let _parser = parser.clone();
  let mut sem_analyzer = semantic_analyzer::SemanticAnalyzer::new(_parser);
  sem_analyzer.analyze();

  let mut interpreter = interpreter::build_interpreter(parser);
  let result = interpreter.interpret();

  global_memory::display();
  println!("Result: {:#?}", result);  
}