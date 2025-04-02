type token =
  | INT of (int)
  | IF
  | THEN
  | ELSE
  | FOR
  | WHILE
  | DOT_3
  | TRUE
  | FALSE
  | INPUT
  | PRINT
  | VECTOR
  | MATRIX
  | TRANSPOSE
  | DETERMINANT
  | DIM_V
  | DIM_1_M
  | DIM_2_M
  | ASSIGN
  | EQUAL
  | LT
  | GT
  | LE
  | GE
  | NE
  | NOT
  | AND
  | OR
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | SEMICOLON
  | EOF
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | ERR_STR of (string)
  | FLOAT of (float)
  | STRING of (string)
  | IDENT of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
