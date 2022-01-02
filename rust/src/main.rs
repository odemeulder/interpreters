#[macro_use]
extern crate lazy_static;

use std::fs;

mod lexer;
mod symbol_table;
mod global_scope;
mod parser;
mod node_visitor;
mod interpreter;
mod semantic_analyzer;

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------

fn insert() {
  global_scope::insert("olivier", lexer::TokenValue::Int(48));
}

fn main() {
  insert();

  println!("ODM Interpreter");
  let progr = fs::read_to_string("program10.txt").unwrap();
  println!("Program: {:#?}", &progr);
  let lexer = lexer::build_lexer(progr);
  let parser = parser::build_parser(lexer);

  let _parser = parser.clone();
  let mut sem_analyzer = semantic_analyzer::SemanticAnalyzer::new(_parser);
  symbol_table::display();
  sem_analyzer.analyze();
  symbol_table::display();

  let mut interpreter = interpreter::build_interpreter(parser);
  let result = interpreter.interpret();
  global_scope::display();
  println!("Result: {:#?}", result);  
}