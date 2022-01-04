
//--------------------------------------------------------------------
//               G L O B A L    M E M O R Y
//--------------------------------------------------------------------

use std::collections::HashMap;
use crate::lexer::TokenValue;

use std::sync::Mutex;lazy_static! {
  static ref GLOBAL_MEMORY: Mutex<HashMap<&'static str, TokenValue>> = Mutex::new(HashMap::new());
}

pub fn get(s: &'static str) -> TokenValue {
  match GLOBAL_MEMORY.lock().unwrap().get(s) {
    None => TokenValue::None,
    Some(t) => t.clone()
  }
}

pub fn insert(s: &'static str, t: TokenValue) {
  GLOBAL_MEMORY.lock().unwrap().insert(s, t);
}

pub fn display() {
  println!("Global Memory: {:#?}", GLOBAL_MEMORY.lock().unwrap())
}

