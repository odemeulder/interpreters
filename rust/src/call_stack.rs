//--------------------------------------------------------------------
//               C A L L   S T A C K
//--------------------------------------------------------------------
use std::collections::HashMap;
use crate::datum::Datum;

pub struct CallStack {
  stack: Vec<StackFrame>
}

impl CallStack {

  pub fn new() -> Self {
    CallStack {
      stack: Vec::new()
    }
  }

  pub fn push(&mut self, frame: StackFrame) {
    &self.stack.push(frame);
  }

  pub fn pop(&mut self) -> Option<StackFrame> {
    self.stack.pop()
  }

  pub fn get(&mut self, s: &'static str) -> Datum {
    let len = self.stack.len();
    if len == 0 {
      return Datum::None
    }
    match self.stack[len - 1].members.get(s) {
      None => Datum::None,
      Some(t) => t.clone()
    }
  }
  
  pub fn insert(&mut self, s: &'static str, t: Datum)  {
    let len = self.stack.len();
    if len == 0 {
      return 
    }
    self.stack[len - 1].members.insert(s, t);
  }

  pub fn display(&mut self) {
    println!("-------------------------");
    println!("CALL STACK");
    println!("-------------------------");
    let len = self.stack.len();
    for i in 0..len {
      &self.stack[i].display();
    }  
  }

}

#[derive(Clone)]
pub struct StackFrame {
  name: &'static str,
  level: u32,
  frame_type: StackFrameType,
  members: HashMap<&'static str, Datum>
}

impl StackFrame {

  pub fn new(name: &'static str, level: u32, frame_type: StackFrameType) -> Self {
    let members: HashMap<&'static str, Datum> = HashMap::new();
    StackFrame {
      name,
      level,
      frame_type,
      members
    }
  }

  pub fn display(&mut self) {
    println!("Stack Frame: {} (level {})", &self.name, &self.level);
    println!("{:?}", &self.members)
  }
}

#[derive(Clone)]
pub enum StackFrameType {
  Program,
  Procedure
}