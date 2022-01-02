//--------------------------------------------------------------------
//               G L O B A L   S C O P E
//--------------------------------------------------------------------

use std::collections::HashMap;
use std::sync::Mutex;
use crate::lexer;

lazy_static! {
  static ref GLOBAL_SCOPE: Mutex<HashMap<&'static str, lexer::TokenValue>> = Mutex::new(HashMap::new());
}

pub fn insert(s: &'static str, t: lexer::TokenValue) {
  let mut guard = GLOBAL_SCOPE.lock().unwrap();
  guard.insert(s, t);
}

pub fn retrieve(s: &'static str) -> lexer::TokenValue {
  let map =  GLOBAL_SCOPE.lock().unwrap();
  return match map.get(&s) {
    None => lexer::TokenValue::None,
    Some(v) => v.clone()
  };
}

pub fn display() {
  println!("Global scope {:#?}", GLOBAL_SCOPE.lock().unwrap());
}
