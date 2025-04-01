%{
open Ast
%}

%token <int> INT
%token IF THEN ELSE FOR WHILE DOT_3 
%token TRUE FALSE
%token INPUT PRINT  
%token VECTOR MATRIX 
  %token TRANSPOSE DETERMINANT
%token DIM_V DIM_1_M DIM_2_M
  %token ASSIGN
   %token EQUAL LT GT LE GE NE NOT AND OR
    %token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
     %token COMMA SEMICOLON 
  %token EOF
%token PLUS MINUS MULT DIV MOD
 %token <string> ERR_STR
%token <float> FLOAT 
  %token <string> STRING
%token <string> IDENT

%start prog
%type <Ast.expr> prog

                 
%right ASSIGN
%left OR
%left AND
%left NOT
%left EQUAL NE
%left LT LE GT GE
%left PLUS MINUS
%left MULT DIV MOD
%left SEMICOLON
%left COMMA  
%left LPAREN RPAREN
%left LBRACE RBRACE
%left LBRACKET RBRACKET 
%nonassoc WHILE FOR  
%nonassoc IF THEN ELSE


%%

prog:
  expr EOF { $1 }
;;

expr:
  INT { Int $1 }
| STRING { String $1 }
| FLOAT { Float $1 }
| expr PLUS expr {Binop(Add, $1, $3)}
| expr MINUS expr {Binop(Sub, $1, $3)}
| expr MULT expr {Binop(Mul, $1, $3)}
| expr DIV expr {Binop(Div, $1, $3)}
| expr MOD expr {Binop(Mod, $1, $3)}
| TRUE { Bool true }
| FALSE { Bool false }
| expr AND expr {Boolop (And, $1, $3)}
| expr OR expr { Boolop (Or, $1, $3) }
| NOT expr {Boolop (Not, $2, Bool false) }
| IF expr THEN expr ELSE expr { IfElse($2, $4, $6) } 
| IDENT { Var $1 } 
| expr ;ASSIGN ; expr { Assign($1, $3) }
| LPAREN expr RPAREN { Paren($2) }
| IDENT LPAREN args RPAREN { Func($1, $3)}
| LBRACE expr_list_semicolon RBRACE { Block($2) }
| expr LT expr {Binop(Lt, $1 , $3)}
| expr GT expr {Binop(Gt, $1 , $3)}
| expr LE expr {Binop(Le, $1 , $3)}
| expr GE expr {Binop(Ge, $1 , $3)}
| expr NE expr {Binop(Ne, $1 , $3)}
| expr EQUAL expr {Binop(Eq, $1 , $3)}
| WHILE expr LBRACE expr_list_semicolon RBRACE { WhileLoop($2, ($4)) }
| FOR IDENT ASSIGN INT DOT_3 INT LBRACE expr_list_semicolon RBRACE { ForLoop($2, (Int $4),(Int $6), $8) }
  | DIM_1_M LPAREN expr RPAREN { Dim1($3) }
  | DIM_2_M LPAREN expr RPAREN { Dim2($3) }
  | DIM_V LPAREN expr RPAREN { VecDim($3) }
  | VECTOR LBRACKET expr_list_comma RBRACKET { Vector($3) }
  | MATRIX LBRACKET mat_row_list RBRACKET { Matrix($3) }
  | TRANSPOSE LPAREN expr RPAREN { Transpose($3) }
  | DETERMINANT LPAREN expr RPAREN { Det($3) }
  | INPUT LPAREN expr RPAREN { Inp($3) } 
  | PRINT LPAREN expr RPAREN { Print($3) }
  | IDENT LBRACKET expr RBRACKET { Vec_ix($1 ,$3) }
args: expr_list_comma 
    expr { $1 }
  | expr COMMA args { $1 :: $3 } 
 
expr_list_semicolon:     
  | expr SEMICOLON expr_list_semicolon { $1 :: $3 }
  | expr { [$1] }
  ;

expr_list_comma:     
  | expr COMMA expr_list_comma { $1 :: $3 }
| expr COMMA expr { [$1; $3] }
| expr { [$1] }
| { [] }
  ;
mat_row_list:
    LBRACKET expr_list_semicolon RBRACKET { [$2] }  
  | LBRACKET expr_list_semicolon RBRACKET SEMICOLON mat_row_list { $2 :: $5 } 
  | LBRACKET RBRACKET { []  }
;
; 
