// use std::fs;
use std::env;
use std::process;

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------

fn main() {

  let args: Vec<String> = env::args().collect();

  let config = odm_interpreter::Config::new(&args).unwrap_or_else(|err| {
      println!("Problem parsing arguments: {}", err);
      process::exit(1);
  });

  println!("Parsing program file {}", config.filename);

  if let Err(e) = odm_interpreter::run(config) {
      
      println!("Application error: {}", e);

      process::exit(1);
  }
}