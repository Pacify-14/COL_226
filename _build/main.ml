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

let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | _ -> failwith "Precondition violated: Not a final value"

let is_val : expr -> bool = function
  | Int _ | Float _ | Bool _ | String _ -> true
  | _ -> false

let rec step (e : expr) : expr =
  match e with
  | Int _ | Float _ | Bool _ | String _ -> failwith "Does not step further"
  | Binop (bop, e1, e2) ->
      if is_val e1 && is_val e2 then step_bop bop e1 e2
      else if is_val e1 then Binop (bop, e1, step e2)
      else Binop (bop, step e1, e2)
  | _ -> failwith "Unhandled case in step function"

and step_bop bop v1 v2 =
  match bop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | Add, Float a, Float b -> Float (a +. b)
  | Sub, Int a, Int b -> Int (a - b)
  | Sub, Float a, Float b -> Float (a -. b)
  | Mul, Int a, Int b -> Int (a * b)
  | Mul, Float a, Float b -> Float (a *. b)
  | Div, Int a, Int b when b <> 0 -> Int (a / b)
  | Div, Float a, Float b when b <> 0.0 -> Float (a /. b)
  | Mod, Int a, Int b when b <> 0 -> Int (a mod b)
  | Eq, Int a, Int b -> Bool (a = b)
  | Eq, Float a, Float b -> Bool (a = b)
  | Lt, Int a, Int b -> Bool (a < b)
  | Lt, Float a, Float b -> Bool (a < b)
  | Gt, Int a, Int b -> Bool (a > b)
  | Gt, Float a, Float b -> Bool (a > b)
  | Le, Int a, Int b -> Bool (a <= b)
  | Le, Float a, Float b -> Bool (a <= b)
  | Ge, Int a, Int b -> Bool (a >= b)
  | Ge, Float a, Float b -> Bool (a >= b)
  | _ -> failwith "Invalid operation or mismatched types"

let rec eval (e : expr) : expr =
  if is_val e then e else eval (step e)

let interp (s : string) : string =
  try
    s |> parse |> eval |> string_of_val
  with Failure msg -> "Runtime error: " ^ msg


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
      print_endline ("Parsed AST: " ^ string_of_expr ast);
      (* Use eval to fully reduce the AST *)
      let result = eval ast in
      print_endline ("Evaluated: " ^ string_of_expr result);
      repl ()  (* Continue reading input *)
    )
  with
    End_of_file -> ()  (* Stop reading on EOF *)
  | Parsing.Parse_error -> print_endline "Syntax error"; repl ()
  | Failure msg -> print_endline ("Error: " ^ msg); repl ()

let () = repl ()  (* Start the REPL loop *)

