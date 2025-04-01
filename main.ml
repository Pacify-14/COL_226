(*open Ast

(* parses to AST*)
let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

let string_of_val (e: expr) : string =
  match e with 
  | Int i -> string_of_int i

let interp (s: string) : string = 
  s |> parse |> string_of_val *)

open Ast

(* Parses input to AST and prints it *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Convert AST to a readable string representation *)
let rec string_of_expr (e: expr) : string =
  match e with
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Float fl -> "Float(" ^ string_of_float fl ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Bool false -> "Bool(false)"
  | Bool true -> "Bool(true)"
  | Binop (Add, e1, e2) -> "Binop(+, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Sub, e1, e2) -> "Binop(-," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Mul, e1, e2) -> "Binop(*," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Mod, e1, e2) -> "Binop(%," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 
  | Binop (Div, e1, e2) -> "Binop(/," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 
  | Binop (Lt, e1, e2) -> "Binop(<," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Gt, e1, e2) -> "Binop(>," ^ string_of_expr e2 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Le, e1, e2) -> "Binop(<=," ^ string_of_expr e2 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Ge, e1, e2) -> "Binop(>=," ^ string_of_expr e2 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Ne, e1, e2) -> "Binop(!=," ^ string_of_expr e2 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Eq, e1, e2) -> "Binop(=," ^ string_of_expr e2 ^ ", " ^ string_of_expr e2 ^ ")"

  | Boolop (And, e1, e2) -> "Boolop(And, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Boolop (Or, e1, e2) -> "Boolop(Or, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Boolop (Not, e1, e2) -> "Boolop(Not, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| IfElse (cond, e1, e2) -> "if " ^ string_of_expr cond ^ " then " ^ string_of_expr e1 ^ " else "^ string_of_expr e2
| Var id -> id
| Assign(var , value) -> "Assign(" ^string_of_expr var ^ " ," ^ string_of_expr value ^ ")"
| Paren e -> "(" ^ string_of_expr e ^ ")"
| Func (f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr args ) ^ ")"

| Block(exprs) -> "{" ^ String.concat "; " (List.map string_of_expr exprs) ^ " }"
| ForLoop (var, start_expr, end_expr, body) -> "for "^var ^ " = " ^ string_of_expr start_expr ^ " ... " ^ string_of_expr end_expr ^ " { " ^ String.concat "; " (List.map string_of_expr body) ^ " }"
| WhileLoop (cond, body) -> "while " ^ string_of_expr cond ^ " { " ^ String.concat "; " (List.map string_of_expr body) ^ " }" 
| Vector lst -> "Vector [" ^ String.concat ", " (List.map string_of_expr lst) ^ "]"
  | Matrix rows -> 
      "Matrix [" ^ String.concat "; " 
        (List.map (fun row -> "[" ^ String.concat ", " (List.map string_of_expr row) ^ "]") rows) ^ "]"
  | Transpose e -> "Transpose(" ^ string_of_expr e ^ ")"
  | Det e -> "Det(" ^ string_of_expr e ^ ")"
  | Dim1 e -> "Dim1(" ^ string_of_expr e ^ ")"
  | Dim2 e -> "Dim2(" ^ string_of_expr e ^ ")"
  | VecDim e -> "VecDim(" ^ string_of_expr e ^ ")"
  | Inp e -> "Inp(" ^ string_of_expr e ^ ")"
  | Print e -> "Print(" ^ string_of_expr e ^ ")"
  | Vec_ix(var, exp) -> var ^ "[" ^ string_of_expr exp ^ "]"
let rec repl () =
  try
    let input = read_line () in
    if input = "exit" then () (* Allows user to exit gracefully *)
    else (
      let ast = parse input in
      print_endline (string_of_expr ast);
      repl ()  (* Recursively call repl to keep reading input *)
    )
  with
    End_of_file -> ()  (* Stop reading on EOF *)
  | Parsing.Parse_error -> print_endline "Syntax error"; repl() 
let () = repl ()  (* Start the REPL loop *)

