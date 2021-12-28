use std::io;

// Note: to debug:
// println!("token {:#?}", token);

//--------------------------------------------------------------------
//               L E X E R
//--------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
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
//--------------------------------------------------------------------
//               P A R S E R
//--------------------------------------------------------------------

struct BinOp {
  token: Token,
  left: Box<dyn AstNode>,
  right: Box<dyn AstNode>,
}
struct Num {
  value: Option<String>
}


#[derive(Debug)]
struct Parser {
  lexer: Lexer,
  current_token: Option<Token>
}

fn build_parser(lexer: Lexer) -> Parser {
  return Parser {
    lexer,
    current_token: None
  }
}
impl Parser {
  fn error(&self) ->() {
    panic!("Error parsing input")
  }

  fn eat(&mut self, _token_type: TokenType) -> () {
    match &self.current_token {
      Some(Token { token_type: _token_type, ..}) => self.current_token = Some(self.lexer.get_next_token()),
      _ => self.error(),
    }
  }

  fn factor(&mut self) -> Box<dyn AstNode> {
    match &self.current_token {
      Some(token) if token.token_type == TokenType::Integer => {
        let _token = token.clone();
        self.eat(TokenType::Integer);
        return Box::new(Num {
          value: _token.value
        })
      },
      Some(token) if token.token_type == TokenType::Lparen => {
        self.eat(TokenType::Lparen);
        let node = self.expr();
        self.eat(TokenType::Rparen);
        return node;
      }
      _ => { self.error(); return Box::new(Num { value: Some(String::from("0"))}) }
    }
  }

  fn term(&mut self) -> Box<dyn AstNode> {
    
    let mut node = self.factor();

    loop {
      match &self.current_token {
        None => break,
        Some(token) => { 
          let _token = token.clone();
          match token.token_type {
            TokenType::Mul => self.eat(TokenType::Mul),
            TokenType::Div => self.eat(TokenType::Div),
            _ => break
          }
          node = Box::new(BinOp {
            left: node,
            token: _token,
            right: self.factor()
          })          
        }
      }
    }
    return node;
  }

  fn expr(&mut self) -> Box<dyn AstNode> {
    // If this is the the initial run, the current_token is None
    match self.current_token {
      None => self.current_token = Some(self.lexer.get_next_token()),
      _ => ()
    }
    // 
    let mut node = self.term();
    loop {
      match &self.current_token {
        None => break,
        Some(token) => { 
          let _token = token.clone();
          match token.token_type {
            TokenType::Plus => self.eat(TokenType::Plus),
            TokenType::Minus => self.eat(TokenType::Minus),
            _ => break
          }
          node = Box::new(BinOp {
            left: node,
            token: _token,
            right: self.term()
          })          
        }
      }
    }
    return node;
  }

  fn parse(&mut self) -> Box<dyn AstNode> {
    return self.expr()
  }
}

//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

trait AstNode {
  fn visit_node(&self) -> u32;
}

impl AstNode for BinOp {
  fn visit_node(&self) -> u32 {
    match self.token.token_type {
      TokenType::Plus  => self.left.visit_node() + self.right.visit_node(),
      TokenType::Minus => self.left.visit_node() - self.right.visit_node(),
      TokenType::Mul   => self.left.visit_node() * self.right.visit_node(),
      TokenType::Div   => self.left.visit_node() / self.right.visit_node(),
      _ => 99999,
    }
  }
}

impl AstNode for Num {
  fn visit_node(&self) -> u32 {
    return match &self.value {
      None => 99999,
      Some(v) => match v.parse::<u32>() {
        Ok(n) => n,
        Err(_) => 99999
      }
    }
  }
}

#[derive(Debug)]
struct Interpreter {
  parser: Parser
}

fn build_interpreter(parser: Parser) -> Interpreter {
  return Interpreter {
    parser
  }
}

impl Interpreter {
  fn interpret(&mut self) -> u32 {
    let tree = &self.parser.parse();
    return tree.visit_node();
  }
}

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------

fn main() {
  println!("ODM Interpreter");
  loop {
    let mut progr = String::new();
    io::stdin()
        .read_line(&mut progr)
        .expect("Failed to read line");
    println!("Program: {}", &progr);
    let lexer = build_lexer(progr);
    let parser = build_parser(lexer);
    let mut interpreter = build_interpreter(parser);
    let result = interpreter.interpret();
    println!("Result: {}", result);  
  }
}