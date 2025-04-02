type bop = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt | Gt | Le | Ge | Ne | Eq

type boolop = 
  And 
  | Or
  | Not

type expr =
  | Int of int 
  | Float of float
  | String of string
  | Binop of bop * expr * expr 
  | Inp of expr
  | Print of expr 
  | Assign of expr * expr      
  | Bool of bool
  | Boolop of boolop * expr * expr
  | IfElse of expr * expr * expr 
  | Var of string
  | Func of string * expr list
  | Paren of expr
  | Block of expr list
  | WhileLoop of expr * expr list
  | ForLoop of string * expr * expr  *expr list
  | Vector of expr list
  | Matrix of expr list list
  | Transpose of expr
  | Det of expr
  | Dim1 of expr
  | Dim2 of expr
  | VecDim of expr
  | Vec_ix of string * expr
