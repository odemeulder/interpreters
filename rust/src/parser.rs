//--------------------------------------------------------------------
//               P A R S E R
//--------------------------------------------------------------------

use crate::lexer::Lexer;
use crate::lexer::build_token; // todox
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::TokenValue;
use crate::node_visitor::AstNode;

pub struct Program {
  name: String,
  pub block: Box<dyn AstNode>,
}

pub struct ProcedureDecl {
  name: &'static str,
  pub block: Box<dyn AstNode>,
}

pub struct Block {
  pub declarations: Vec<Box<dyn AstNode>>,
  pub compound_statement: Box<dyn AstNode>
}

pub struct BinOp {
  pub token: Token,
  pub left: Box<dyn AstNode>,
  pub right: Box<dyn AstNode>,
}

pub struct Num {
  pub value: TokenValue
}

pub struct UnaryOp {
  pub token: Token,
  pub expr: Box<dyn AstNode>,
}

pub struct Compound {
  pub children: Vec<Box<dyn AstNode>>
}

pub struct Assign {
  pub left: &'static str,
  pub right: Box<dyn AstNode>,
}

#[derive(Clone, Debug)]
pub struct Var {
  pub value: TokenValue
}

pub fn build_var(token: Token) -> Var {
  Var { value: token.value }
}

pub struct VarDecl {
  pub var_node: Var,
  pub type_node: Type
}

#[derive(Clone, Debug)]
pub struct Type {
  pub token: Token,
  pub value: TokenValue
}

pub struct NoOp;

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

  pub fn parse(&mut self) -> Box<dyn AstNode> {
    let node = self.program();
    match &self.current_token.token_type {
      TokenType::Eof => {},
      _ => self.error()
    }
    return node;
  }
}
