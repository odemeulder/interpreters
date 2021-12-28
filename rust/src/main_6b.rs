use std::io;

// Note: to debug:
// println!("token {:#?}", token);

#[derive(Clone, Debug, PartialEq)]
enum TokenType {
  Integer,
  Plus,
  Minus,
  Mul,
  Div,
  Lparen,
  Rparen,
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

#[derive(Clone, Debug)]
struct Lexer {
  text: String,
  pos: usize,
  current_char: Option<char>,
}

fn build_lexer(_text: String) -> Lexer {
  let first_char = _text.chars().next();
  return Lexer {
    text: _text,
    pos: 0,
    current_char: first_char,
  }
}

impl Lexer {
  
  fn error(&self) ->() {
    panic!("Invalid character")
  }

  fn advance(&mut self) -> () {
    self.pos += 1;
    if self.pos > self.text.chars().count() - 1 {
      self.current_char = None;
    } else {
      self.current_char = Some(self.text.chars().nth(self.pos).unwrap());
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
        Some(c) if c == '*' => {
          self.advance(); 
          return build_token(TokenType::Mul, Some(String::from('*')))
        },
        Some(c) if c == '/' => {
          self.advance(); 
          return build_token(TokenType::Div, Some(String::from('/')))
        },
        Some(c) if c == '(' => {
          self.advance(); 
          return build_token(TokenType::Lparen, Some(String::from('(')))
        },
        Some(c) if c == ')' => {
          self.advance(); 
          return build_token(TokenType::Rparen, Some(String::from(')')))
        },
        _ => self.error()
      }
    }
  }
}

#[derive(Debug)]
struct Interpreter {
  lexer: Lexer,
  current_token: Option<Token>,
}

fn build_interpreter(_lexer: Lexer) -> Interpreter {
  return Interpreter {
    lexer: _lexer,
    current_token: None,
  }
}

impl Interpreter {

  fn error(&self) ->() {
    panic!("Error parsing input")
  }

  fn eat(&mut self, _token_type: TokenType) -> () {
    match &self.current_token {
      Some(Token { token_type: _token_type, ..}) => self.current_token = Some(self.lexer.get_next_token()),
      _ => self.error(),
    }
  }

  fn factor(&mut self) -> u32 {
    match &self.current_token {
      Some(token) if token.token_type == TokenType::Integer => {
        let _token = token.clone();
        self.eat(TokenType::Integer);
        return match _token.value {
          None => { self.error(); return 0 },
          Some(v) => v.parse().unwrap()
        }
      },
      Some(token) if token.token_type == TokenType::Lparen => {
        self.eat(TokenType::Lparen);
        let result = self.expr();
        self.eat(TokenType::Rparen);
        return result;
      }
      _ => { self.error(); return 0 }
    }
  }

  fn term(&mut self) -> u32 {
    
    let mut result: u32 = self.factor();

    loop {
      match &self.current_token {
        Some(token) if token.token_type == TokenType::Mul => {
          self.eat(TokenType::Mul);
          result = result * self.term();
        },
        Some(token) if token.token_type == TokenType::Div => {
          self.eat(TokenType::Div);
          result = result / self.term()
        },
        _ => break
      }
    }
    return result;
  }

  fn expr(&mut self) -> u32 {
    // If this is the the initial run, the current_token is None
    match self.current_token {
      None => self.current_token = Some(self.lexer.get_next_token()),
      _ => ()
    }
    // 
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
  }
}


fn main() {
  println!("ODM Interpreter");
  loop {
    let mut progr = String::new();
    io::stdin()
        .read_line(&mut progr)
        .expect("Failed to read line");
    println!("Program: {}", &progr);
    let lexer = build_lexer(progr);
    let mut interpreter = build_interpreter(lexer);
    let result = interpreter.expr();
    println!("Result: {}", result);  
  }
}