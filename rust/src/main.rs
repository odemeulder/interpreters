#[macro_use]
extern crate lazy_static;

// use std::io;
use std::fs;
use std::collections::HashMap;
use std::sync::Mutex;

// Note: to debug:
// println!("token {:#?}", token);

lazy_static! {
  static ref GLOBAL_SCOPE: Mutex<HashMap<String, Option<i32>>> = Mutex::new(HashMap::new());
}

//--------------------------------------------------------------------
//               L E X E R
//--------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenType {
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
  Integer_Const,
  Real_Const,
  Integer_Div,
  Float_Div
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

  fn peek(&mut self) -> Option<char> {
    let peek_pos = self.pos + 1;
    if peek_pos > self.text.chars().count() - 1 {
      return None;
    } else {
      return Some(self.text.chars().nth(peek_pos).unwrap());
    }
  }

  fn _id(&mut self) -> Token {
    let mut reserved_keywords: HashMap<String, Token> = HashMap::new();
    reserved_keywords.insert(String::from("BEGIN"), build_token(TokenType::Begin, Some(String::from("BEGIN"))));
    reserved_keywords.insert(String::from("END"), build_token(TokenType::End, Some(String::from("END"))));
    reserved_keywords.insert(String::from("PROGRAM"), build_token(TokenType::Program, Some(String::from("PROGRAM"))));
    reserved_keywords.insert(String::from("VAR"), build_token(TokenType::Var, Some(String::from("VAR"))));
    reserved_keywords.insert(String::from("INTEGER"), build_token(TokenType::Integer, Some(String::from("INTEGER"))));
    reserved_keywords.insert(String::from("REAL"), build_token(TokenType::Real, Some(String::from("REAL"))));
    reserved_keywords.insert(String::from("DIV"), build_token(TokenType::Integer_Div, Some(String::from("DIV"))));
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
    return match reserved_keywords.get(&result) {
      None => build_token(TokenType::Id, Some(result)),
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
      self.advance(); // the closing curly brace
    }
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
      return build_token(TokenType::Real_Const, Some(result)) 
    } else {
      return build_token(TokenType::Integer_Const, Some(result))
    };
  }

  fn get_next_token(&mut self) -> Token {
    loop {
      match self.current_char {
        None => return build_token(TokenType::Eof, None),
        Some(c) if c.is_whitespace() => self.skip_whitespace(),
        Some(c) if c == '{' => {
          self.advance();
          self.skip_comment();
        }
        Some(c) if c.is_digit(10) => return self.number(),
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
          return build_token(TokenType::Float_Div, Some(String::from('/')))
        },
        Some(c) if c == '(' => {
          self.advance(); 
          return build_token(TokenType::Lparen, Some(String::from('(')))
        },
        Some(c) if c == ')' => {
          self.advance(); 
          return build_token(TokenType::Rparen, Some(String::from(')')))
        },
        Some(c) if c.is_alphanumeric() => {
          return self._id(); 
        },
        Some(c) if c == ':' => {
          if let Some('=') = &self.peek() {
            self.advance(); 
            self.advance();
            return build_token(TokenType::Assign, Some(String::from(":=")))  
          }
        },
        Some(c) if c == ';' => {
          self.advance(); 
          return build_token(TokenType::Semi, Some(String::from(';')))
        },
        Some(c) if c == '.' => {
          self.advance(); 
          return build_token(TokenType::Dot, Some(String::from('.')))
        },
        Some(c) if c == ':' => {
          self.advance(); 
          return build_token(TokenType::Colon, Some(String::from(':')))
        },
        Some(c) if c == ',' => {
          self.advance(); 
          return build_token(TokenType::Comma, Some(String::from(',')))
        },
        _ => self.error()
      }
    }
  }
}
//--------------------------------------------------------------------
//               P A R S E R
//--------------------------------------------------------------------

struct Program {
  name: String,
  block: Box<dyn AstNode>,
}
struct Block {
  declarations: Vec<VarDecl>,
  compound_statement: Box<dyn AstNode>
}
struct BinOp {
  token: Token,
  left: Box<dyn AstNode>,
  right: Box<dyn AstNode>,
}
struct Num {
  value: Option<String>
}
struct UnaryOp {
  token: Token,
  expr: Box<dyn AstNode>,
}
struct Compound {
  children: Vec<Box<dyn AstNode>>
}
struct Assign {
  left: String,
  right: Box<dyn AstNode>,
}
struct Var {
  value: Option<String>
}

fn build_var(token: Option<Token>) -> Var {
  match token {
    Some(t) => Var { value: t.value },
    None => Var { value: None }
  }
}

struct VarDecl {
  var_node: Var,
  type_node: Type
}
#[derive(Clone, Debug)]
struct Type {
  token: Token,
  value: String
}
struct NoOp;

// Parser
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
    println!("token {:#?}", &self.current_token);
    match &self.current_token {
      Some(Token { token_type: _token_type, ..}) => self.current_token = Some(self.lexer.get_next_token()),
      _ => self.error(),
    }
  }

  fn factor(&mut self) -> Box<dyn AstNode> {
    // factor : PLUS  factor
    //        | MINUS factor
    //        | INTEGER_CONST
    //        | REAL_CONST
    //        | LPAREN expr RPAREN
    //        | variable
    match &self.current_token {
      None => self.error(),
      Some(token) => {
        let _token = token.clone();
        match &token.token_type {
          TokenType::Plus | TokenType::Minus => {
            self.eat(_token.token_type);
            return Box::new(UnaryOp {
              token: _token,
              expr: self.factor()
            })
          },
          TokenType::Integer => {
            let _token = token.clone();
            self.eat(TokenType::Integer_Const);
            return Box::new(Num {
              value: _token.value
            })
          },
          TokenType::Real => {
            let _token = token.clone();
            self.eat(TokenType::Real_Const);
            return Box::new(Num {
              value: _token.value
            })
          },
          TokenType::Lparen => {
            self.eat(TokenType::Lparen);
            let node = self.expr();
            self.eat(TokenType::Rparen);
            return node;            
          },
          _ => return self.variable()
        }
      }
    }
    match &self.current_token {
      Some(token) if token.token_type == TokenType::Integer_Const => {
        let _token = token.clone();
        self.eat(TokenType::Integer_Const);
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
    // term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
    let mut node = self.factor();
    loop {
      match &self.current_token {
        None => break,
        Some(token) => { 
          let _token = token.clone();
          match token.token_type {
            TokenType::Mul => self.eat(TokenType::Mul),
            TokenType::Integer_Div => self.eat(TokenType::Integer_Div),
            TokenType::Float_Div => self.eat(TokenType::Float_Div),
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

    // Begin
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

  fn program(&mut self) -> Box<dyn AstNode> {
    // If this is the the initial run, the current_token is None
    match self.current_token {
      None => self.current_token = Some(self.lexer.get_next_token()),
      _ => ()
    }
    // program : compound_statement DOT"
    self.eat(TokenType::Program);
    let var_node = self.variable();
    let prog_name = var_node.value.unwrap(); // TODO
    self.eat(TokenType::Semi);
    let block_node = self.block();
    let program_node = Box::new(Program {
      block: block_node,
      name: prog_name
    });
    self.eat(TokenType::Dot);
    return program_node;
  }

  fn block(&mut self) -> Box<dyn AstNode> {
    // block : declarations compound_statement
    let declaration_nodes = self.declarations();
    let compound_node = self.compound_statement();
    return Box::new(Block {
      compound_statement: compound_node,
      declarations: declaration_nodes
    })
  }

  fn declarations(&mut self) -> Vec<VarDecl>  {
    // declarations : VAR (variable_declaration SEMI)+
    //              | empty
    let mut declarations: Vec<VarDecl> = Vec::new();
    loop {
      match &self.current_token {
        Some(token) if token.token_type == TokenType::Var => self.eat(TokenType::Var),
        Some(token) if token.token_type == TokenType::Id => {
          let mut var_declarations = self.variable_declaration();
          declarations.append(&mut var_declarations);
          self.eat(TokenType::Semi);
        },
        _ => break
      }
    }
    return declarations;
  }

  fn variable_declaration(&mut self) -> Vec<VarDecl> {
    // variable_declaration : ID (COMMA ID)* COLON type_spec

    let mut var_nodes = vec![build_var(self.current_token.clone())];
    loop {
      match &self.current_token {
        Some(t) if t.token_type == TokenType::Comma => {
          self.eat(TokenType::Comma);
          var_nodes.push(build_var(self.current_token.clone()));
          self.eat(TokenType::Id);
        },
        _ => break
      }
    }
    self.eat(TokenType::Colon);
    let type_node = self.type_spec();
    let mut var_declarations = vec![];
    for var_node in var_nodes {
      var_declarations.push(VarDecl {
        var_node: var_node,
        type_node: type_node.clone()
      })
    }
    return var_declarations;
  }

  fn type_spec(&mut self) -> Type {
    // type_spec : INTEGER
    //           | REAL
    match &self.current_token {
      Some(t) if t.token_type == TokenType::Integer => self.eat(TokenType::Integer),
      Some(t) if t.token_type == TokenType::Real => self.eat(TokenType::Real),
      _ => (),
    }
    return match &self.current_token {
      Some(token) => {
        let _token = token.clone();
        let _value = token.clone().value.unwrap(); // TODO
        Type {
          token: _token,
          value: _value 
        }        
      },
      _ => panic!("Error with type_spec")
    }
  }

  fn compound_statement(&mut self) -> Box<dyn AstNode> {
    // compound_statement: BEGIN statement_list END
    self.eat(TokenType::Begin);
    let statements = self.statement_list();
    self.eat(TokenType::End);
    return Box::new(Compound{ children: statements});
  }

  fn statement_list(&mut self) -> Vec<Box<dyn AstNode>> {
    // statement_list : statement
    //                | statement SEMI statement_list
    let node = self.statement();
    let mut results = vec![node];
    loop {
      match &self.current_token {
        None => break,
        Some(token) => {
          match token.token_type {
            TokenType::Semi => {
              self.eat(TokenType::Semi);
              results.push(self.statement())
            },
            _ => break
          }
        }
      }
    }
    return results;
  }

  fn statement(&mut self) -> Box<dyn AstNode> {
    // statement : compound_statement
    //           | assignment_statement
    //           | empty
    match &self.current_token {
      Some(token) => {
        let _token = token.clone();
        match token.token_type {
          TokenType::Begin => self.compound_statement(),
          TokenType::Id => self.assignment_statement(),
          _ => self.empty()
        }
      }
      None => self.empty()
    }
  }

  fn assignment_statement(&mut self) -> Box<dyn AstNode> {
    let variable = self.variable();
    let _token = self.current_token.clone();
    self.eat(TokenType::Assign);
    let right = self.expr();
    match _token {
      None => Box::new(NoOp),
      Some(_) => Box::new(Assign {
        left: variable.value.unwrap(),
        right: right
      })
    }
  }

  fn variable(&mut self) -> Box<Var> {
    let node: Box<Var>;
    if let Some(token) = &self.current_token {
      let _val = token.clone().value;
      node = Box::new(Var {
        value: _val     
      });
      self.eat(TokenType::Id);
      return node;
    }
    panic!("Unable to assign variable");
  }

  fn empty(&mut self) -> Box<dyn AstNode> {
    Box::new(NoOp {})
  }

  fn parse(&mut self) -> Box<dyn AstNode> {
    let node = self.program();
    match &self.current_token {
      Some(token) if token.token_type == TokenType::Eof => {},
      _ => self.error()
    }
    return node;
  }
}

//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

trait AstNode {
  fn visit_node(&self) -> Option<i32>;
}

impl AstNode for BinOp {
  fn visit_node(&self) -> Option<i32> {
    match (self.left.visit_node(), self.right.visit_node()) {
      (Some(l), Some(r)) => match self.token.token_type {
                              TokenType::Plus  => Some(l+r),
                              TokenType::Minus => Some(l-r),
                              TokenType::Mul   => Some(l*r),
                              TokenType::Integer_Div   => Some(l/r), // TODO FLOAT DIV
                              _ => panic!("Unexpected token"),
                            }
      _ => panic!("Unexpected token")
    }

  }
}

impl AstNode for Num {
  fn visit_node(&self) -> Option<i32> {
    return match &self.value {
      None => panic!("Invalid node"),
      Some(v) => match v.parse::<i32>() {
        Ok(n) => Some(n),
        Err(_) => panic!("Cannot convert token to int")
      }
    }
  }
}

impl AstNode for UnaryOp {
  fn visit_node(&self) -> Option<i32> {
    match self.expr.visit_node() {
      None => None,
      Some(i) => match self.token.token_type {
        TokenType::Plus  => Some(i),
        TokenType::Minus => Some(-i),
        _ => panic!("Invalid unary operator")
      }
    }
  }
}

impl AstNode for Compound {
  fn visit_node(&self) -> Option<i32> {
    for statement in &self.children {
      statement.visit_node();
    }
    return None;
  }
}

impl AstNode for Assign {
  fn visit_node(&self) -> Option<i32> {
    let var_name = &self.left;
    let right = self.right.visit_node();
    let mut guard = GLOBAL_SCOPE.lock().unwrap();
    guard.insert(var_name.to_string(), right);
    None 
  }
}

impl AstNode for Var {
  fn visit_node(&self) -> Option<i32> {
    let var_name = self.value.as_ref().unwrap(); // TODO
    let map =  GLOBAL_SCOPE.lock().unwrap();
    let x = match map.get(var_name) {
      None => None,
      Some(v) => *v
    };
    return x;
  }
}

impl AstNode for Block {
  fn visit_node(&self) -> Option<i32> {
    // for decl in self.declarations {
    //   decl.visit_node();
    // } // SINCE visit VarDecl does nothing for now. leaving this out.
    self.compound_statement.visit_node()
  }
}

impl AstNode for VarDecl {
  fn visit_node(&self) -> Option<i32> {
    None // TO DO 
  }
}
impl AstNode for Type {
  fn visit_node(&self) -> Option<i32> {
    None // TO DO 
  }
}

impl AstNode for Program {
  fn visit_node(&self) -> Option<i32> {
    return self.block.visit_node();
  }
}

impl AstNode for NoOp {
  fn visit_node(&self) -> Option<i32> {
    None 
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
  fn interpret(&mut self) -> Option<i32> {
    let tree = &self.parser.parse();
    return tree.visit_node();
  }
}

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------

fn insert() {
  GLOBAL_SCOPE.lock().unwrap().insert(String::from("olivier"), Some(48));
}

fn retrieve() -> Option<i32> {
  let map = GLOBAL_SCOPE.lock().unwrap();
  let result = map.get("olivier");
  println!("Result {:#?}", result);
  if let Some(i) = result {
    return *i;
  }
  panic!("Could not retrieve");
}

fn main() {
  insert();
  let r = retrieve();
  println!("r: {:#?}", r);

  println!("ODM Interpreter");
  let progr = fs::read_to_string("program10.txt").unwrap();
  println!("Program: {:#?}", &progr);
  let lexer = build_lexer(progr);
  let parser = build_parser(lexer);
  let mut interpreter = build_interpreter(parser);
  let result = interpreter.interpret();
  println!("Global scope {:#?}", GLOBAL_SCOPE.lock().unwrap());
  println!("Result: {:#?}", result);  
}