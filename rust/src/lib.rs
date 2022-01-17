use std::error::Error;
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


pub struct Config {
    pub filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        
        if args.len() < 2 {
            return Err("not enough arguments");
        }

        let filename = args[1].clone();

        Ok(Config { filename })
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {

    println!("ODM Interpreter");
    let progr = fs::read_to_string(config.filename).unwrap();
    println!("Program: {:#?}", &progr);
    let lexer = lexer::build_lexer(progr);
    let parser = parser::build_parser(lexer);
  
    let _parser = parser.clone();
    let mut sem_analyzer = semantic_analyzer::SemanticAnalyzer::new(_parser);
    sem_analyzer.analyze();
  
    let mut interpreter = interpreter::build_interpreter(parser);
    let result = interpreter.interpret();
  
    println!("Result: {:#?}", result);  
  
    Ok(())
}
