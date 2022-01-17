//--------------------------------------------------------------------
//               P A R S E R
//--------------------------------------------------------------------

use crate::symbol::ProcSymbol;
use crate::lexer::Lexer;
use crate::lexer::build_token; 
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::TokenValue;
use crate::node_visitor::AstNode;
use std::rc::Rc;
use std::fmt;

static LOG_PARSER: bool = false;

pub struct Program {
  pub name: String,
  pub block: Box<dyn AstNode>
}
impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "Program, name {}", self.name)
  }
}

pub struct ProcedureDecl {
  pub name: &'static str,
  pub block_ref: Rc<dyn AstNode>,
  // pub block: Box<dyn AstNode>,
  pub params: Vec<Box <dyn AstNode>>,
}
impl fmt::Display for ProcedureDecl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "ProcedureDecl, name {}", self.name)
  }
}

pub struct Block {
  pub declarations: Vec<Box<dyn AstNode>>,
  pub compound_statement: Box<dyn AstNode>
}
impl fmt::Display for Block {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "Block [")?;
      // Following code causing stack overflow
      // for d in &self.declarations { 
      //   write!(f, "<Declaration {} >,", d)?;
      // };
      write!(f, "]")?;
      Ok(())
  }
}


pub struct BinOp {
  pub token: Token,
  pub left: Box<dyn AstNode>,
  pub right: Box<dyn AstNode>,
}
impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "BinOp {}", self.token.token_type)
  }
}

pub struct Num {
  pub value: TokenValue
}
impl fmt::Display for Num {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Num {}", self.value)
  }
}

pub struct UnaryOp {
  pub token: Token,
  pub expr: Box<dyn AstNode>,
}
impl fmt::Display for UnaryOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "UnaryOp {}", self.token.token_type)
  }
}

pub struct Compound {
  pub children: Vec<Box<dyn AstNode>>
}
impl fmt::Display for Compound {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Compound [")?;
    for c in &self.children {
      write!(f, "<Child: {} >, ", c)?
    }
    write!(f, "]")?;
    Ok(())
  }
}

pub struct Assign {
  pub left: &'static str,
  pub right: Box<dyn AstNode>,
}
impl fmt::Display for Assign {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Assign {}", self.left)
  }
}

#[derive(Clone, Debug)]
pub struct Var {
  pub value: TokenValue
}
impl fmt::Display for Var {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Var {}", self.value)
  }
}

pub fn build_var(token: Token) -> Var {
  Var { value: token.value }
}

#[derive(Clone, Debug)]
pub struct VarDecl {
  pub var_node: Var,
  pub type_node: Type
}
impl fmt::Display for VarDecl {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "VarDecl {}", self.var_node)
  }
}

#[derive(Clone, Debug)]
pub struct Type {
  pub token: Token,
  pub value: TokenValue
}
impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Type {}", self.value)
  }
}
#[derive(Clone, Debug)]
pub struct Param {
  pub var_node: Var,
  pub type_node: Type
}
impl fmt::Display for Param {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Param {}", self.var_node)
  }
}
pub struct ProcCall {
  pub proc_name: &'static str,
  pub args: Vec<Box<dyn AstNode>>,
  pub token: Token,
  pub proc_symbol: Option<ProcSymbol>
}
impl fmt::Display for ProcCall {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "ProcCall {}", &self.proc_name)
  }
}

#[derive(Clone, Debug)]
pub struct NoOp;
impl fmt::Display for NoOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "NoOp")
  }
}

#[derive(Clone, Debug)]
pub struct Strink {
  pub token: Token,
  pub value: TokenValue
}
impl fmt::Display for Strink {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Strink {}", self.value)
  }
}

#[derive(Clone, Debug)]
pub struct Boolean {
  pub token: Token,
  pub value: TokenValue
}
impl fmt::Display for Boolean {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Boolean {}", self.value)
  }
}

pub struct WriteStatement {
  pub new_line: bool,
  pub content: Box<dyn AstNode>,
}
impl fmt::Display for WriteStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "WriteStatement")
  }
}


// Parser
#[derive(Debug, Clone)]
pub struct Parser {
  pub lexer: Lexer,
  current_token: Token
}

pub fn build_parser(lexer: Lexer) -> Parser {
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
    if LOG_PARSER {
      println!("eat token {:#?}", &self.current_token);
    }
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
    //        | STRING
    //        | variable
    let _token = self.current_token.clone();
    let __token = self.current_token.clone(); // hacky
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
      TokenType::String => {
        self.eat(TokenType::String);
        return Box::new(Strink {
          token: _token,
          value: __token.value
        })
      }
      TokenType::True => {
        self.eat(TokenType::True);
        return Box::new(Boolean {
          token: _token,
          value: TokenValue::Bool(true)
        })
      }
      TokenType::False => {
        self.eat(TokenType::False);
        return Box::new(Boolean {
          token: _token,
          value: TokenValue::Bool(false)
        })
      }
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
    self.relation()
  }

  fn relation(&mut self) -> Box<dyn AstNode> {
    /*
    relation : arithmetic_expr (rel_op arithmetic_expr)?
    rel_op   : LESS_THAN
              | GREATER_THAN
              | EQUAL
              | LESS_EQUAL
              | GREATER_EQUAL
              | NOT_EQUAL
    */
    let mut node = self.arithmetic_expr();
    let _token = self.current_token.clone();
    match self.current_token.token_type {
      TokenType::Equal => self.eat(TokenType::Equal),
      TokenType::NotEqual => self.eat(TokenType::NotEqual),
      TokenType::GreaterThan => self.eat(TokenType::GreaterThan),
      TokenType::GreaterEqual => self.eat(TokenType::GreaterEqual),
      TokenType::LessThan => self.eat(TokenType::LessThan),
      TokenType::LessEqual => self.eat(TokenType::LessEqual),
      _ => return node // if no relational operator, then we return the current node
    }
    node = Box::new(BinOp {
      left: node,
      token: _token,
      right: self.arithmetic_expr()
    });
    return node;
  }

  fn arithmetic_expr(&mut self) -> Box<dyn AstNode> {
    /* arithmetic_expr : term ((PLUS | MINUS) term)* */
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

  fn block_ref(&mut self) -> Rc<dyn AstNode> {
    // block : declarations compound_statement
    let declaration_nodes = self.declarations();
    let compound_node = self.compound_statement();
    return Rc::new(Block {
      compound_statement: compound_node,
      declarations: declaration_nodes
    })
  }

  fn declarations(&mut self) -> Vec<Box<dyn AstNode>>  {
    /* declarations : (VAR (variable_declaration SEMI)+)+ procedure_declaration*
    */
    let mut declarations: Vec<Box<dyn AstNode>> = Vec::new();
    loop {
      match self.current_token.token_type {
        TokenType::Var => {
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
        },
        _ => break
      }
    }
    loop {
      match self.current_token.token_type {
        TokenType::Procedure => {
          let mut proc_declarations = self.procedure_declarations();
          declarations.append(&mut proc_declarations);
        },
        _ => break
      }
    }
    return declarations
  }

  fn procedure_declarations(&mut self) -> Vec<Box<dyn AstNode>> {
    /* procedure_declaration :
         PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
    */
    let mut declarations: Vec<Box<dyn AstNode>> = Vec::new();
    self.eat(TokenType::Procedure);
    let proc_name = match self.current_token.value {
      TokenValue::String(s) => s,
      _ => panic!("Unexpected token type after procedure declaration.")
    };
    self.eat(TokenType::Id);

    let mut params: Vec<Box<dyn AstNode>> = Vec::new();
    if self.current_token.token_type == TokenType::Lparen {
      self.eat(TokenType::Lparen);
      params = self.formal_parameter_list();
      self.eat(TokenType::Rparen);
    }

    self.eat(TokenType::Semi);
    // let block_node = self.block();
    let block_ref = self.block_ref();
    let proc_decl = Box::new(ProcedureDecl {
      name: proc_name,
      // block: block_node,
      block_ref,
      params
    });
    declarations.push(proc_decl);
    self.eat(TokenType::Semi);  
    return declarations;
  }

  fn formal_parameter_list(&mut self) -> Vec<Box<dyn AstNode>> {
    /* formal_parameter_list : formal_parameters
                             | formal_parameters SEMI formal_parameter_list
    */
    if !(self.current_token.token_type == TokenType::Id) {
      return vec![];
    }
    let mut params = self.formal_parameters();
    if self.current_token.token_type == TokenType::Semi {
      self.eat(TokenType::Semi);
      params.append(&mut self.formal_parameter_list());
    }
    return params;
  }

  fn formal_parameters(&mut self) -> Vec<Box<dyn AstNode>> {
    /* formal_parameters: ID (COMMA ID)* COLON type_spec
    */
    let mut param_names = vec![build_var(self.current_token.clone())];
    self.eat(TokenType::Id);
    loop {
      match self.current_token.token_type {
        TokenType::Comma => {
          self.eat(TokenType::Comma);
          let param_node = build_var(self.current_token.clone());
          param_names.push(param_node);
          self.eat(TokenType::Id);
        },
        _ => break
      }
    }
    self.eat(TokenType::Colon);
    let type_node = self.type_spec();
    let mut params: Vec<Box<dyn AstNode>> = Vec::new();
    for param_name in param_names {
      params.push(Box::new(Param {
        var_node: param_name,
        type_node: type_node.clone()
      }))
    }
    return params;
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
    //           | BOOLEAN
    let _token = self.current_token.clone();
    let _value = self.current_token.clone().value;
    match &self.current_token.token_type {
      TokenType::Integer => self.eat(TokenType::Integer),
      TokenType::Real => self.eat(TokenType::Real),
      TokenType::Boolean => self.eat(TokenType::Boolean),
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
    /* statement :  compound_statement
                  | write_statement
                  | proc_call_statement
                  | assignment_statement
                  | empty
    */
    match self.current_token.token_type {
      TokenType::Begin => self.compound_statement(),
      TokenType::Write | TokenType::Writeln => self.write_statement(),
      TokenType::Id if self.lexer.get_curr_char() == Some('(') => self.proccall_statement(), // hacky
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
    let ret_val = Box::new(Assign {
      left: var_name,
      right: right
    });
    return ret_val;
  }

  fn variable(&mut self) -> Box<Var> {
    let node: Box<Var> = Box::new(Var {
      value: self.current_token.clone().value     
    });
    self.eat(TokenType::Id);
    return node;
  }

  fn proccall_statement(&mut self) -> Box<dyn AstNode> {
    /* proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN */
    let proc_name = match self.current_token.value {
      TokenValue::String(s) => s,
      _ => panic!("Parser error: Token type error for proc call")
    };
    self.eat(TokenType::Id);
    self.eat(TokenType::Lparen);
    let mut args: Vec<Box<dyn AstNode>> = Vec::new();
    loop {
      match self.current_token.token_type {
        TokenType::Rparen => { 
          self.eat(TokenType::Rparen);
          break; 
        }
        TokenType::Comma => { self.eat(TokenType::Comma); }
        _ => {
          let node = self.expr();
          args.push(node);
        }
      }
    }
    let return_node = Box::new(ProcCall {
      proc_name,
      args,
      token: self.current_token.clone(),
      proc_symbol: None
    });
    return return_node;
  }

  fn write_statement(&mut self) -> Box<dyn AstNode> {
    /* write_statement : WRITE | WRITELN (LPAREN (expr (SEMI expr)*)? RPAREN)? */
    let new_line: bool = match self.current_token.token_type {
      TokenType::Writeln => {
        self.eat(TokenType::Writeln);
        true
      },
      TokenType::Write => {
        self.eat(TokenType::Write);
        false
      },
      _ => panic!("Parser error: Token type for write statement should be Write or Writeln")
    };
    self.eat(TokenType::Lparen);
    let content = self.expr();
    let rv = Box::new(WriteStatement {
      new_line,
      content
    });
    self.eat(TokenType::Rparen);
    return rv;
  }

  fn empty(&mut self) -> Box<dyn AstNode> {
    Box::new(NoOp {})
  }

  pub fn parse(&mut self) -> Box<dyn AstNode> {
    let node = self.program();
    match &self.current_token.token_type {
      TokenType::Eof => {},
      _ => self.error()
    }
    return node;
  }
}
