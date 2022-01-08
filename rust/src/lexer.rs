//--------------------------------------------------------------------
//               L E X E R
//--------------------------------------------------------------------

use std::fmt;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
  None,
  Plus,
  Minus,
  Mul,
  Lparen,
  Rparen,
  Eof,
  Begin,
  End,
  Assign,
  Semi,
  Dot,
  Id,
  Program,
  Colon,
  Comma,
  Integer,
  Real,
  Var,
  IntegerConst,
  RealConst,
  IntegerDiv,
  FloatDiv,
  Procedure
}

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self)
  }
}

#[derive(Clone, Debug)]
pub enum TokenValue {
  None,
  String(&'static str),
  Int(i32),
  Float(f64),
}

impl fmt::Display for TokenValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self)
  }
}

#[derive(Clone, Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub value: TokenValue,
}

pub fn build_token(token_type: TokenType, value: TokenValue) -> Token {
  Token {
    token_type,
    value,
  }
}

#[derive(Clone, Debug)]
pub struct Lexer {
  text: String,
  pos: usize,
  current_char: Option<char>,
  line_no: u32,
  col_no: u32,
}

pub fn build_lexer(_text: String) -> Lexer {
  let first_char = _text.chars().next();
  return Lexer {
    text: _text,
    pos: 0,
    current_char: first_char,
    line_no: 0,
    col_no: 0,
  }
}

macro_rules! hashmap {
  ($( $key: expr => $val: expr ),*) => {{
       let mut map = HashMap::new();
       $( map.insert(String::from($key), build_token($val, TokenValue::String($key))); )*
       map
  }}
}

impl Lexer {
  
  fn error(&self) ->() {
    panic!("Invalid character {:?} line: {:?} col: {:?}", self.current_char, self.line_no, self.col_no)
  }

  fn advance(&mut self) -> () {
    if self.current_char == Some('\n') {
      self.line_no += 1;
      self.col_no = 0;
    }

    self.pos += 1;
    if self.pos > self.text.chars().count() - 1 {
      self.current_char = None;
    } else {
      self.current_char = Some(self.text.chars().nth(self.pos).unwrap());
      self.col_no += 1;
    }
  }

  fn peek(&mut self) -> Option<char> {
    let peek_pos = self.pos + 1;
    if peek_pos > self.text.chars().count() - 1 {
      return None;
    } else {
      return Some(self.text.chars().nth(peek_pos).unwrap());
    }
  }

  fn _id(&mut self) -> Token {
    let keywords = hashmap![
      "BEGIN"     => TokenType::Begin,
      "END"       => TokenType::End,
      "PROGRAM"   => TokenType::Program,
      "VAR"       => TokenType::Var,
      "INTEGER"   => TokenType::Integer,
      "REAL"      => TokenType::Real,
      "DIV"       => TokenType::IntegerDiv,
      "PROCEDURE" => TokenType::Procedure
    ];
    let mut result = String::default();
    loop {
      match self.current_char {
        Some(c) if c.is_alphanumeric() => {
          result.push(c);
          self.advance();
        },
        _ => break,
      }
    }
    let _result = result.clone();
    let s_slice: &str = Box::leak(_result.into_boxed_str()); // Strange concoction to convert String to &'static str
    return match keywords.get(&result.to_uppercase()) {
      None => build_token(TokenType::Id, TokenValue::String(s_slice)),
      Some(token) => token.clone()
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

  fn skip_comment(&mut self) -> () {
    loop {
      match self.current_char {
        Some(c) if ! (c == '}') => self.advance(),
        _ => break
      }
    }
    self.advance(); // the closing curly brace
  }

  fn number(&mut self) -> Token {
    let mut result = String::default();
    let mut is_real: bool = false;
    loop {
      match self.current_char {
        Some(c) if c.is_digit(10) => {
          result.push(c);
          self.advance();
        },
        Some(c) if c == '.' => {
          result.push(c);
          is_real = true;
          self.advance();
        }
        _ => break,
      }
    }
    if is_real { 
      let f: f64 = result.parse().unwrap(); // unwrap is safe here, considering we just built the string correctly above
      return build_token(TokenType::RealConst, TokenValue::Float(f)) 
    } else {
      let i: i32 = result.parse().unwrap(); // unwrap is safe here, considering we just built the string correctly above
      return build_token(TokenType::IntegerConst, TokenValue::Int(i)) 
    };
  }

  pub fn get_curr_char(&mut self) -> Option<char> {
    self.current_char
  }

  pub fn get_next_token(&mut self) -> Token {
    loop {
      match self.current_char {
        None => return build_token(TokenType::Eof, TokenValue::None),
        Some(c) if c.is_whitespace() => self.skip_whitespace(),
        Some(c) if c == '{' => {
          self.advance();
          self.skip_comment();
        }
        Some(c) if c.is_digit(10) => return self.number(),
        Some(c) if c.is_alphanumeric() => {
          return self._id(); 
        },
        Some(c) if c == ':' => {
          if let Some('=') = &self.peek() {
            self.advance(); 
            self.advance();
            return build_token(TokenType::Assign, TokenValue::String(":="))  
          } else {
            self.advance(); 
            return build_token(TokenType::Colon, TokenValue::String(":"))  
          }
        },
        Some(c) if c == '+' => {
          self.advance(); 
          return build_token(TokenType::Plus, TokenValue::String("+"))
        },
        Some(c) if c == '-' => {
          self.advance(); 
          return build_token(TokenType::Minus, TokenValue::String("-'"))
        },
        Some(c) if c == '*' => {
          self.advance(); 
          return build_token(TokenType::Mul, TokenValue::String("*"))
        },
        Some(c) if c == '/' => {
          self.advance(); 
          return build_token(TokenType::FloatDiv, TokenValue::String("/"))
        },
        Some(c) if c == '(' => {
          self.advance(); 
          return build_token(TokenType::Lparen, TokenValue::String("("))
        },
        Some(c) if c == ')' => {
          self.advance(); 
          return build_token(TokenType::Rparen, TokenValue::String(")"))
        },
        Some(c) if c == ';' => {
          self.advance(); 
          return build_token(TokenType::Semi, TokenValue::String(";"))
        },
        Some(c) if c == '.' => {
          self.advance(); 
          return build_token(TokenType::Dot, TokenValue::String("."))
        },
        Some(c) if c == ',' => {
          self.advance(); 
          return build_token(TokenType::Comma, TokenValue::String(","))
        },
        _ => self.error()
      }
    }
  }
}
