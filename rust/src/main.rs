use std::fs;

mod lexer;
mod symbol;
mod parser;
mod node_visitor;
mod interpreter;
mod semantic_analyzer;
mod scope;
mod call_stack;
mod datum;

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------


fn main() {

  println!("ODM Interpreter");
  let progr = fs::read_to_string("program19.txt").unwrap();
  println!("Program: {:#?}", &progr);
  let lexer = lexer::build_lexer(progr);
  let parser = parser::build_parser(lexer);

  let _parser = parser.clone();
  let mut sem_analyzer = semantic_analyzer::SemanticAnalyzer::new(_parser);
  sem_analyzer.analyze();

  let mut interpreter = interpreter::build_interpreter(parser);
  let result = interpreter.interpret();

  println!("Result: {:#?}", result);  
}