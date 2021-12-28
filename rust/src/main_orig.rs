use std::io;

// Note: to debug:
// println!("token {:#?}", token);

#[derive(Clone, Debug, PartialEq)]
enum TokenType {
  Integer,
  Plus,
  Minus,
  Eof,
}

#[derive(Clone, Debug)]
struct Token {
  token_type: TokenType,
  value: Option<String>,
}

fn build_token(token_type: TokenType, value: Option<String>) -> Token {
  Token {
    token_type,
    value,
  }
}

#[derive(Debug)]
struct Interpreter {
  text: String,
  pos: usize,
  current_token: Option<Token>,
  current_char: Option<char>,
}

fn build_interpreter(_text: String) -> Interpreter {
  let first_char = _text.chars().next();
  return Interpreter {
    text: _text,
    pos: 0,
    current_token: None,
    current_char: first_char,
  }
}

impl Interpreter {
  
  fn error(&self) ->() {
    panic!("Error parsing input")
  }

  fn advance(&mut self) -> () {
    self.pos += 1;
    if self.pos > self.text.chars().count() - 1 {
      self.current_char = None;
    } else {
      self.current_char = Some(self.text.chars().nth(self.pos).unwrap());
    }
  }

  fn eat(&mut self, _token_type: TokenType) -> () {
    match &self.current_token {
      Some(Token { token_type: _token_type, ..}) => self.current_token = Some(self.get_next_token()),
      _ => self.error(),
    }
  }

  fn skip_whitespace(&mut self) -> () {
    loop {
      match self.current_char {
        Some(c) if c.is_whitespace() => self.advance(),
        _ => break,
      }
    }
  }

  fn term(&mut self) -> u32 {
    let token = self.current_token.clone().unwrap();
    self.eat(token.token_type);
    match token.value {
      None => 0,
      Some(v) => v.parse().unwrap()
    }
  }

  fn integer(&mut self) -> String {
    let mut result = String::default();
    loop {
      match self.current_char {
        Some(c) if c.is_digit(10) => {
          result.push(c);
          self.advance();
        },
        _ => break,
      }
    }
    return result;
  }

  fn get_next_token(&mut self) -> Token {
    loop {
      match self.current_char {
        None => return build_token(TokenType::Eof, None),
        Some(c) if c.is_whitespace() => self.skip_whitespace(),
        Some(c) if c.is_digit(10) => return build_token(TokenType::Integer, Some(self.integer())),
        Some(c) if c == '+' => {
          self.advance(); 
          return build_token(TokenType::Plus, Some(String::from('+')))
        },
        Some(c) if c == '-' => {
          self.advance(); 
          return build_token(TokenType::Minus, Some(String::from('-')))
        },
        _ => self.error()
      }
    }
  }

  fn expr(&mut self) -> u32 {
    self.current_token = Some(self.get_next_token());
    let mut result: u32 = self.term();
    loop {
      match &self.current_token {
        Some(token) if token.token_type == TokenType::Plus => {
          self.eat(TokenType::Plus);
          result = result + self.term();
        },
        Some(token) if token.token_type == TokenType::Minus => {
          self.eat(TokenType::Minus);
          result = result - self.term()
        },
        _ => break
      }
    }
    return result;

    // let left = self.current_token.clone().unwrap();
    // self.eat(TokenType::Integer);
    // let op = self.current_token.clone().unwrap();
    // let opval = op.clone().value.unwrap().chars().next().unwrap();
    // if opval == '+' {
    //   self.eat(TokenType::Plus);
    // } else {
    //   self.eat(TokenType::Minus);
    // }
    // let right = self.current_token.clone().unwrap();
    // self.eat(TokenType::Integer);
    // match (left.value, op.value, right.value) {
    //   (Some(leftval), Some(op), Some(rightval))  
    //     => {
    //       let int1: u32 = leftval.parse().unwrap();
    //       let int2: u32 = rightval.parse().unwrap();
    //       return match op.as_str() {
    //         "+" => int1 + int2,
    //         _ => int1 - int2,
    //       }
    //     },
    //   _ => panic!("parsing error"),
    // }
  }
}


fn main() {
  println!("ODM Interpreter");
  
  let mut progr = String::new();
  io::stdin()
      .read_line(&mut progr)
      .expect("Failed to read line");
  println!("Program: {}", &progr);
  let mut interpreter = build_interpreter(progr);
  let result = interpreter.expr();
  println!("Result: {}", result);
}