############################################
#    L E X E R                             #
############################################
INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = 'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', '(', ')', 'EOF'

class Token(object):

  def __init__(self, type, value) -> None:
      super().__init__()
      self.type = type
      self.value = value

  def __str__(self) -> str:
      return 'Token({type}, {value})'.format(type=self.type, value=repr(self.value))

  def __repr__(self) -> str:
      return self.__str__()

class Lexer(object):
  def __init__(self, text):
    self.text = text
    self.pos = 0
    self.current_char = self.text[self.pos]

  def error(self):
    raise Exception('Invalid character')

  def advance(self):
    self.pos += 1
    if self.pos > len(self.text) - 1:
      self.current_char = None
    else: 
      self.current_char = self.text[self.pos]
  
  def skip_whitespace(self):
    while self.current_char is not None and self.current_char.isspace():
      self.advance()

  def integer(self):
    result = ''
    while self.current_char is not None and self.current_char.isdigit():
      result += self.current_char
      self.advance()
    return int(result)

  def get_next_token(self):
    while self.current_char is not None:
      if self.current_char.isspace():
        self.skip_whitespace()
        continue
      if self.current_char.isdigit():
        return Token(INTEGER, self.integer())
      if self.current_char == '+':
        self.advance()
        return Token(PLUS, '+')      
      if self.current_char == '-':
        self.advance()
        return Token(MINUS, '*')    
      if self.current_char == '*':
        self.advance()
        return Token(MUL, '/')    
      if self.current_char == '/':
        self.advance()
        return Token(DIV, '-')    
      if self.current_char == '(':
        self.advance()
        return Token(LPAREN, '(')    
      if self.current_char == ')':
        self.advance()
        return Token(RPAREN, ')')    
      self.error()
    return Token(EOF, None) 

############################################
#    P A R S E R                           #
############################################
class AST(object):
  pass

class BinOp(AST):
  def __init__(self, left, op, right) -> None:
    self.left = left
    self.op = op 
    self.token = op
    self.right = right

class Num(AST):
  def __init__(self, token) -> None:
    self.token = token
    self.value = token.value

class UnaryOp(AST):
  def __init__(self, token, expr) -> None:
    self.token = self.op = token
    self.expr = expr

class Parser(object):
  def __init__(self,lexer) -> None:
    self.lexer = lexer
    self.current_token = self.lexer.get_next_token()

  def error(self):
    raise Exception('Invalid syntax')

  def eat(self, token_type):
    if self.current_token.type == token_type:
      self.current_token = self.lexer.get_next_token()
    else:
      self.error()

  def factor(self):
    token = self.current_token
    if token.type == PLUS:
      self.eat(PLUS)
      return UnaryOp(token, self.factor())
    if token.type == MINUS:
      self.eat(MINUS)
      return UnaryOp(token, self.factor())
    if token.type == INTEGER: 
      self.eat(INTEGER)
      return Num(token)
    elif token.type == LPAREN:
      self.eat(LPAREN)
      node = self.expr()
      self.eat(RPAREN)
      return node

  def term(self):
    node = self.factor()
    while self.current_token.type in (MUL, DIV):
      token = self.current_token
      if token.type == MUL:
        self.eat(MUL)
      elif token.type == DIV:
        self.eat(DIV)
      node = BinOp(left=node, op=token, right=self.factor())
    return node

  def expr(self):
    node = self.term()
    while self.current_token.type in (PLUS, MINUS):
      token = self.current_token
      if token.type == PLUS:
        self.eat(PLUS)
      elif token.type == MINUS:
        self.eat(MINUS)
      node = BinOp(left=node, op=token, right=self.term())
    return node
  
  def parse(self):
    return self.expr()

############################################
#    I N T E R P R E T E R                 #
############################################
class NodeVisitor(object):
  def visit(self, node):
    method_name = 'visit_' + type(node).__name__
    visitor = getattr(self, method_name, self.generic_visit)
    return visitor(node)

  def generic_visit(self, node):
    raise Exception('No visit_{} method'.format(type(node).__name__))

class Interpreter(NodeVisitor):
  def __init__(self, parser):
    self.parser = parser

  def visit_BinOp(self, node):
    if node.op.type == PLUS:
      return self.visit(node.left) + self.visit(node.right)
    if node.op.type == MINUS:
      return self.visit(node.left) - self.visit(node.right)
    if node.op.type == MUL:
      return self.visit(node.left) * self.visit(node.right)
    if node.op.type == DIV:
      return self.visit(node.left) / self.visit(node.right)

  def visit_Num(self, node):
    return node.value

  def visit_UnaryOp(self, node):
    op = node.op.type
    if op == PLUS:
      return +self.visit(node.expr)
    elif op == MINUS:
      return -self.visit(node.expr)
  
  def interpret(self):
    tree = self.parser.parse()
    return self.visit(tree)


def main():
  while True:
    try:
      text = input('spi > ')
    except EOFError:
      break
    if not text:
      continue
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    print(result)

if __name__ == '__main__':
  main()