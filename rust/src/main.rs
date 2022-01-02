#[macro_use]
extern crate lazy_static;

// use std::io;
use std::fs;
use std::fmt;
use std::collections::HashMap;
use std::sync::Mutex;

// Note: to debug:
// println!("token {:#?}", token);

lazy_static! {
  static ref GLOBAL_SCOPE: Mutex<HashMap<String, TokenValue>> = Mutex::new(HashMap::new());
}

//--------------------------------------------------------------------
//               L E X E R
//--------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenType {
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
enum TokenValue {
  None,
  String(&'static str),
  Int(i32),
  Float(f64),
}

#[derive(Clone, Debug)]
struct Token {
  token_type: TokenType,
  value: TokenValue,
}

fn build_token(token_type: TokenType, value: TokenValue) -> Token {
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
    panic!("Invalid character {:#?}", self.current_char)
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
    reserved_keywords.insert(String::from("BEGIN"), build_token(TokenType::Begin, TokenValue::String("BEGIN")));
    reserved_keywords.insert(String::from("END"), build_token(TokenType::End, TokenValue::String("END")));
    reserved_keywords.insert(String::from("PROGRAM"), build_token(TokenType::Program, TokenValue::String("PROGRAM")));
    reserved_keywords.insert(String::from("VAR"), build_token(TokenType::Var, TokenValue::String("VAR")));
    reserved_keywords.insert(String::from("INTEGER"), build_token(TokenType::Integer, TokenValue::String("INTEGER")));
    reserved_keywords.insert(String::from("REAL"), build_token(TokenType::Real, TokenValue::String("REAL")));
    reserved_keywords.insert(String::from("DIV"), build_token(TokenType::IntegerDiv, TokenValue::String("DIV")));
    reserved_keywords.insert(String::from("PROCEDURE"), build_token(TokenType::IntegerDiv, TokenValue::String("PROCEDURE")));
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
    return match reserved_keywords.get(&result) {
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

  fn get_next_token(&mut self) -> Token {
    loop {
      match self.current_char {
        None => return build_token(TokenType::Eof, TokenValue::None),
        Some(c) if c.is_whitespace() => self.skip_whitespace(),
        Some(c) if c == '{' => {
          self.advance();
          self.skip_comment();
        }
        Some(c) if c.is_digit(10) => return self.number(),
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

//--------------------------------------------------------------------
//               S Y M B O L S
//--------------------------------------------------------------------

#[derive(Clone, Debug)]
struct BuiltinTypeSymbol {
  name: &'static str,
}

impl BuiltinTypeSymbol {
  fn new(s: &'static str) -> BuiltinTypeSymbol {
    BuiltinTypeSymbol {
      name: s,
    }
  }
}

#[derive(Clone, Debug)]
struct VarSymbol {
  name: &'static str,
  symbol_type: BuiltinTypeSymbol,
}
impl VarSymbol {
  fn new(s: &'static str, tt:BuiltinTypeSymbol) -> VarSymbol {
    VarSymbol {
      name: s,
      symbol_type: tt
    }
  }
}

trait NamedSymbol {
  fn get_name(&mut self) -> &'static str;
}

impl NamedSymbol for Symbol {
  fn get_name(&mut self) -> &'static str {
    match &self {
      Symbol::Builtin(b) => b.name,
      Symbol::Var(v) => v.name,
      Symbol::None => "NONE"
    }
  }
}

impl fmt::Display for BuiltinTypeSymbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", self.name)
  }
}
impl fmt::Display for VarSymbol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "<{}:{}>", self.name, self.symbol_type)
  }
}

#[derive(Clone, Debug)]
enum Symbol {
  None,
  Builtin(BuiltinTypeSymbol),
  Var(VarSymbol)
}

lazy_static! {
  static ref SYMBOL_TABLE: Mutex<HashMap<&'static str, Symbol>> = Mutex::new(HashMap::new());
}

fn define_symbol(sym: Symbol) -> () {
  let mut map = SYMBOL_TABLE.lock().unwrap();
  let n = sym.clone().get_name();
  map.insert(n, sym);
}

fn init_symbol_table() -> () {
  define_symbol(Symbol::Builtin(BuiltinTypeSymbol::new("INTEGER")));
  define_symbol(Symbol::Builtin(BuiltinTypeSymbol::new("REAL")));
}

fn lookup_symbol(s: &'static str) -> Symbol {
  let map = SYMBOL_TABLE.lock().unwrap();
  match map.get(s) {
    None => Symbol::None,
    Some(s) => s.clone() 
  }
}

struct SymbolTableBuilder {
  parser: Parser
}
impl SymbolTableBuilder {
  fn new(parser: Parser) -> SymbolTableBuilder {
    init_symbol_table();
    return SymbolTableBuilder {
      parser
    }
  }
  fn build(&mut self) -> () {
    let tree = self.parser.parse();
    tree.visit_node_for_symbols();
  }
}

//--------------------------------------------------------------------
//               P A R S E R
//--------------------------------------------------------------------

struct Program {
  name: String,
  block: Box<dyn AstNode>,
}

struct ProcedureDecl {
  name: &'static str,
  block: Box<dyn AstNode>,
}

struct Block {
  declarations: Vec<Box<dyn AstNode>>,
  compound_statement: Box<dyn AstNode>
}

struct BinOp {
  token: Token,
  left: Box<dyn AstNode>,
  right: Box<dyn AstNode>,
}

struct Num {
  value: TokenValue
}

struct UnaryOp {
  token: Token,
  expr: Box<dyn AstNode>,
}

struct Compound {
  children: Vec<Box<dyn AstNode>>
}

struct Assign {
  left: &'static str,
  right: Box<dyn AstNode>,
}

#[derive(Clone, Debug)]
struct Var {
  value: TokenValue
}

fn build_var(token: Token) -> Var {
  Var { value: token.value }
}

struct VarDecl {
  var_node: Var,
  type_node: Type
}

#[derive(Clone, Debug)]
struct Type {
  token: Token,
  value: TokenValue
}

struct NoOp;

// Parser
#[derive(Debug, Clone)]
struct Parser {
  lexer: Lexer,
  current_token: Token
}

fn build_parser(lexer: Lexer) -> Parser {
  return Parser {
    lexer,
    current_token: build_token(TokenType::None, TokenValue::None)
  }
}

impl Parser {

  fn error(&self) ->() {
    panic!("Error parsing input {:#?}", &self.current_token)
  }

  fn eat(&mut self, _token_type: TokenType) -> () {
    // println!("eat token {:#?}", &self.current_token);
    if self.current_token.token_type == _token_type {
      self.current_token = self.lexer.get_next_token();
    } else {
      self.error();
    }
  }

  fn factor(&mut self) -> Box<dyn AstNode> {
    // factor : PLUS  factor
    //        | MINUS factor
    //        | INTEGER_CONST
    //        | REAL_CONST
    //        | LPAREN expr RPAREN
    //        | variable
    let _token = self.current_token.clone();
    match self.current_token.token_type {
      TokenType::Plus | TokenType::Minus => {
        self.eat(_token.token_type);
        return Box::new(UnaryOp {
          token: _token,
          expr: self.factor()
        })
      },      
      TokenType::IntegerConst => {
        self.eat(TokenType::IntegerConst);
        return Box::new(Num {
          value: _token.value
        })
      },
      TokenType::RealConst => {
        self.eat(TokenType::RealConst);
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

  fn term(&mut self) -> Box<dyn AstNode> {
    // term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
    let mut node = self.factor();
    loop {
      let _token = self.current_token.clone();
      match &self.current_token.token_type {
        TokenType::Mul => self.eat(TokenType::Mul),
        TokenType::IntegerDiv => self.eat(TokenType::IntegerDiv),
        TokenType::FloatDiv => self.eat(TokenType::FloatDiv),
        _ => break
      }
      node = Box::new(BinOp {
        left: node,
        token: _token,
        right: self.factor()
      })          
    }
    return node;
  }

  fn expr(&mut self) -> Box<dyn AstNode> {

    // Begin
    let mut node = self.term();
    loop {
      let _token = self.current_token.clone();
      match &self.current_token.token_type {
        TokenType::Plus => self.eat(TokenType::Plus),
        TokenType::Minus => self.eat(TokenType::Minus),
        _ => break
      };
      node = Box::new(BinOp {
        left: node,
        token: _token,
        right: self.term()
      })          
    }
    return node;
  }

  fn program(&mut self) -> Box<dyn AstNode> {
    // If this is the the initial run, the current_token is None
    if self.current_token.token_type == TokenType::None {
      self.current_token = self.lexer.get_next_token()
    }
    // program : compound_statement DOT"
    self.eat(TokenType::Program);
    let var_node = self.variable();
    let prog_name = match var_node.value {
      TokenValue::String(s) => s,
      _ => "Illegal program name"
    };
    self.eat(TokenType::Semi);
    let block_node = self.block();
    let program_node = Box::new(Program {
      block: block_node,
      name: String::from(prog_name)
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

  fn declarations(&mut self) -> Vec<Box<dyn AstNode>>  {
    /* declarations : VAR (variable_declaration SEMI)+
                     | (PROCEDURE ID SEMI block SEMI)*
                     | empty 
    */
    let mut declarations: Vec<Box<dyn AstNode>> = Vec::new();
    if let TokenType::Var = self.current_token.token_type {
      self.eat(TokenType::Var);
      loop {
        match self.current_token.token_type {
          TokenType::Id => {
            let mut var_declarations = self.variable_declaration();
            declarations.append(&mut var_declarations);
            self.eat(TokenType::Semi);  
          },
          _ => break
        }
      }
    }
    loop {
      match self.current_token.token_type {
        TokenType::Procedure => {
          self.eat(TokenType::Procedure);
          let proc_name = match self.current_token.value {
            TokenValue::String(s) => s,
            _ => panic!("Unexpected token type after procedure declaration.")
          };
          self.eat(TokenType::Id);
          self.eat(TokenType::Semi);
          let block_node = self.block();
          let proc_decl = Box::new(ProcedureDecl {
            name: proc_name,
            block: block_node
          });
          declarations.push(proc_decl);
          self.eat(TokenType::Semi);  
        },
        _ => break
      }
    }
    return declarations
  }

  fn variable_declaration(&mut self) -> Vec<Box<dyn AstNode>> {
    // variable_declaration : ID (COMMA ID)* COLON type_spec
    let mut var_nodes = vec![build_var(self.current_token.clone())];
    self.eat(TokenType::Id);
    loop {
      match &self.current_token.token_type {
        TokenType::Comma => {
          self.eat(TokenType::Comma);
          let var_node = build_var(self.current_token.clone());
          var_nodes.push(var_node);
          self.eat(TokenType::Id);
        },
        _ => break
      }
    }
    self.eat(TokenType::Colon);
    let type_node = self.type_spec();
    let mut var_declarations: Vec<Box<dyn AstNode>> = Vec::new();
    for var_node in var_nodes {
      var_declarations.push(Box::new(VarDecl {
        var_node: var_node,
        type_node: type_node.clone()
      }))
    }
    return var_declarations;
  }

  fn type_spec(&mut self) -> Type {
    // type_spec : INTEGER
    //           | REAL
    let _token = self.current_token.clone();
    let _value = self.current_token.clone().value;
    match &self.current_token.token_type {
      TokenType::Integer => self.eat(TokenType::Integer),
      TokenType::Real => self.eat(TokenType::Real),
      _ => (),
    }
    Type {
      token: _token,
      value: _value 
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
      match &self.current_token.token_type {
        TokenType::Semi => {
          self.eat(TokenType::Semi);
          results.push(self.statement())
        },
        _ => break
      }
    }
    return results;
  }

  fn statement(&mut self) -> Box<dyn AstNode> {
    // statement : compound_statement
    //           | assignment_statement
    //           | empty
    match self.current_token.token_type {
      TokenType::Begin => self.compound_statement(),
      TokenType::Id => self.assignment_statement(),
      _ => self.empty()    
    }
  }

  fn assignment_statement(&mut self) -> Box<dyn AstNode> {
    let variable = self.variable();
    let _token = self.current_token.clone();
    self.eat(TokenType::Assign);
    let right = self.expr();
    let var_name = match variable.value {
      TokenValue::String(s) => s,
      _ => panic!("Illegal variable name")
    };
    return Box::new(Assign {
      left: var_name,
      right: right
    })
  }

  fn variable(&mut self) -> Box<Var> {
    let node: Box<Var> = Box::new(Var {
      value: self.current_token.clone().value     
    });
    self.eat(TokenType::Id);
    return node;
  }

  fn empty(&mut self) -> Box<dyn AstNode> {
    Box::new(NoOp {})
  }

  fn parse(&mut self) -> Box<dyn AstNode> {
    let node = self.program();
    match &self.current_token.token_type {
      TokenType::Eof => {},
      _ => self.error()
    }
    return node;
  }
}

//--------------------------------------------------------------------
//               I N T E R P R E T E R
//--------------------------------------------------------------------

trait AstNode {
  fn visit_node(&self) -> TokenValue;
  fn visit_node_for_symbols(&self);
}

impl AstNode for BinOp {
  fn visit_node(&self) -> TokenValue {
    let left = self.left.visit_node();
    let right = self.right.visit_node();
    return match (left, right) {
      (TokenValue::Int(l), TokenValue::Int(r)) => match self.token.token_type {
                              TokenType::Plus  => TokenValue::Int(l+r),
                              TokenType::Minus => TokenValue::Int(l-r),
                              TokenType::Mul   => TokenValue::Int(l*r),
                              TokenType::IntegerDiv   => TokenValue::Int(l/r), 
                              TokenType::FloatDiv   => TokenValue::Float(l as f64 / r as f64), 
                              _ => panic!("Unexpected binary operator"),
                            },
      (TokenValue::Float(l), TokenValue::Float(r)) => match self.token.token_type {
                              TokenType::Plus  => TokenValue::Float(l+r),
                              TokenType::Minus => TokenValue::Float(l-r),
                              TokenType::Mul   => TokenValue::Float(l*r),
                              TokenType::FloatDiv   => TokenValue::Float(l/r), 
                              _ => panic!("Unexpected binary operator"),
                            },
      _ => panic!("Unexpected token")
    }

  }
  fn visit_node_for_symbols(&self) { 
    self.left.visit_node_for_symbols();
    self.right.visit_node_for_symbols();
  }
}

impl AstNode for Num {
  fn visit_node(&self) -> TokenValue {
    return match self.value {
      TokenValue::None => panic!("Invalid node"),
      TokenValue::Int(i) => TokenValue::Int(i),
      TokenValue::Float(f) => TokenValue::Float(f),
      _ => panic!("Invalid node")
    }
  }
  fn visit_node_for_symbols(&self) { }
}

impl AstNode for UnaryOp {
  fn visit_node(&self) -> TokenValue {
    match self.expr.visit_node() {
      TokenValue::None => TokenValue::None,
      TokenValue::Int(i) => match self.token.token_type {
        TokenType::Plus  => TokenValue::Int(i),
        TokenType::Minus => TokenValue::Int(-i),
        _ => panic!("Invalid unary operator")
      },
      TokenValue::Float(f) => match self.token.token_type {
          TokenType::Plus  => TokenValue::Float(f),
          TokenType::Minus => TokenValue::Float(-f),
          _ => panic!("Invalid unary operator")
        },
      _ => panic!("Invalid unary operator")
    }
  }
  fn visit_node_for_symbols(&self) {
    self.expr.visit_node_for_symbols();
  }
}

impl AstNode for Compound {
  fn visit_node(&self) -> TokenValue {
    for statement in &self.children {
      statement.visit_node();
    }
    return TokenValue::None;
  }
  fn visit_node_for_symbols(&self) { 
    for statement in &self.children {
      statement.visit_node_for_symbols();
    }    
  }
}

impl AstNode for Assign {
  fn visit_node(&self) -> TokenValue {
    let var_name = &self.left;
    let right = self.right.visit_node();
    let mut guard = GLOBAL_SCOPE.lock().unwrap();
    guard.insert(var_name.to_string(), right);
    TokenValue::None 
  }
  fn visit_node_for_symbols(&self) { 
    let var_name = self.left;
    let var_symbol = lookup_symbol(var_name);
    if let Symbol::None = var_symbol {
      panic!("Assign to undeclared variable name: {}", var_name)
    }
    self.right.visit_node_for_symbols()
  }
}

impl AstNode for Var {
  fn visit_node(&self) -> TokenValue {
    // let var_name = self.value.as_ref().unwrap(); // TODO
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Invalid variable name")
    };
    let map =  GLOBAL_SCOPE.lock().unwrap();
    let x = match map.get(var_name) {
      None => TokenValue::None,
      Some(v) => v.clone()
    };
    return x;
  }
  fn visit_node_for_symbols(&self) { 
    let var_name = match self.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot lookup variable, invalid var name type")
    };
    let var_symbol = lookup_symbol(var_name);
    if let Symbol::None = var_symbol {
      panic!("Unknown variable name: {}", var_name)
    }
  }
}

impl AstNode for Block {
  fn visit_node(&self) -> TokenValue {
    // for decl in self.declarations {
    //   decl.visit_node();
    // } // SINCE visit VarDecl does nothing for now. leaving this out.
    self.compound_statement.visit_node()
  }
  fn visit_node_for_symbols(&self) {
    for decl in &self.declarations {
      decl.visit_node_for_symbols();
    } 
    self.compound_statement.visit_node_for_symbols()
  }
}

impl AstNode for VarDecl {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_node_for_symbols(&self) { 
    let type_name = match self.type_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid type name")
    };
    let type_symbol = match lookup_symbol(type_name) {
      Symbol::Builtin(b) => b,
      s => { panic!("Cannot declare variable, invalid built in {:#?}", s); }
    };
    let var_name = match self.var_node.value {
      TokenValue::String(s) => s,
      _ => panic!("Cannot declare variable, invalid var name")
    };
    let var_symbol = VarSymbol::new(var_name, type_symbol);
    define_symbol(Symbol::Var(var_symbol));
  }
}
impl AstNode for Type {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None  
  }
  fn visit_node_for_symbols(&self) { }
}

impl AstNode for Program {
  fn visit_node(&self) -> TokenValue {
    return self.block.visit_node();
  }
  fn visit_node_for_symbols(&self) { 
    self.block.visit_node_for_symbols()
  }
}

impl AstNode for ProcedureDecl {
  fn visit_node(&self) -> TokenValue {
    return TokenValue::None;
  }
  fn visit_node_for_symbols(&self) { 
    ()
  }
}

impl AstNode for NoOp {
  fn visit_node(&self) -> TokenValue {
    TokenValue::None 
  }
  fn visit_node_for_symbols(&self) { }
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
  fn interpret(&mut self) -> TokenValue {
    let tree = &self.parser.parse();
    return tree.visit_node();
  }
}

//--------------------------------------------------------------------
//               M A I N
//--------------------------------------------------------------------

fn insert() {
  GLOBAL_SCOPE.lock().unwrap().insert(String::from("olivier"), TokenValue::Int(48));
}

fn main() {
  insert();

  println!("ODM Interpreter");
  let progr = fs::read_to_string("program10.txt").unwrap();
  println!("Program: {:#?}", &progr);
  let lexer = build_lexer(progr);
  let parser = build_parser(lexer);
  let _parser = parser.clone();
  let mut stb = SymbolTableBuilder::new(_parser);
  println!("Symbol table {:#?}", SYMBOL_TABLE.lock().unwrap());
  stb.build();
  let mut interpreter = build_interpreter(parser);
  let result = interpreter.interpret();
  println!("Global scope {:#?}", GLOBAL_SCOPE.lock().unwrap());
  println!("Symbol table {:?}", SYMBOL_TABLE.lock().unwrap());
  println!("Result: {:#?}", result);  
}