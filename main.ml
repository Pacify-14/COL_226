(* Assume we open or reference Ast so that our AST constructors (Int, Float, Binop, Var, Assign, Block, Print, etc.) are in scope *)
open Lexing
open Ast

(* Assuming Ast module is available and its constructors are as given *)
let rec string_of_expr (e : Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.Float f -> string_of_float f
  | Ast.String s -> "\"" ^ s ^ "\""
  | Ast.Bool b -> string_of_bool b
  | Ast.Var x -> x
  | Ast.Binop (op, e1, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_expr e2 ^ ")"
  | Ast.Assign (e1, e2) ->
      string_of_expr e1 ^ " := " ^ string_of_expr e2
  | Ast.Block es ->
      "{" ^ String.concat "; " (List.map string_of_expr es) ^ "}"
  | Ast.Print e ->
      "Print(" ^ string_of_expr e ^ ")"
  | Ast.WhileLoop (cond, body) ->
      "While(" ^ string_of_expr cond ^ ") {" ^ string_of_expr (Ast.Block body) ^ "}"
  | Ast.ForLoop (var, start, stop, body) ->
      "For(" ^ var ^ " = " ^ string_of_expr start ^ " to " ^ string_of_expr stop ^ ") {" ^ string_of_expr (Ast.Block body) ^ "}"
  | Ast.Transpose e -> "Transpose(" ^ string_of_expr e ^ ")"
  | Ast.Det e -> "Det(" ^ string_of_expr e ^ ")"
  | Ast.Dim1 e -> "Dim1(" ^ string_of_expr e ^ ")"
  | Ast.Dim2 e -> "Dim2(" ^ string_of_expr e ^ ")"
  | Ast.VecDim e -> "VecDim(" ^ string_of_expr e ^ ")"
  | Ast.Vec_ix (x, e) -> x ^ "[" ^ string_of_expr e ^ "]"
  | _ -> "<unknown>"

and string_of_bop op =
  match op with
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "mod"
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
  | Ast.Eq -> "=="
  | Ast.Ne -> "<>"


let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf


(* Global environment: a reference to a list of variable bindings.
   Each binding maps a variable name (string) to an evaluated expression (of type Ast.expr). *)
let env : (string * Ast.expr) list ref = ref []

(* Look up a variable in the environment. *)
let lookup_env (x : string) : Ast.expr =
  try List.assoc x !env 
  with Not_found -> failwith ("Undefined variable: " ^ x)

(* Convert a final evaluated expression to a string.
   Here we assume that final values are of type Int, Float, Bool, or String. *)
let string_of_val (e: Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.Float f -> string_of_float f
  | Ast.Bool b -> string_of_bool b
  | Ast.String s -> "\"" ^ s ^ "\""
  | _ -> failwith "Not a final value"

(* A value is a fully evaluated expression (primitive types only) *)
let is_val : Ast.expr -> bool = function
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ -> true
  | _ -> false

(* Small-step evaluation: perform one reduction step. *)
let rec step (e : Ast.expr) : Ast.expr =
  match e with
  (* Final values do not step further *)
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ -> failwith "Does not step further"

  (* Evaluate binary operations *)
  | Ast.Binop (bop, e1, e2) ->
      if is_val e1 && is_val e2 then step_bop bop e1 e2
      else if is_val e1 then Ast.Binop (bop, e1, step e2)
      else Ast.Binop (bop, step e1, e2)

  (* Assignment: evaluate right-hand side fully, then update the environment *)
  | Ast.Assign (Ast.Var x, e_rhs) ->
      if is_val e_rhs then (
          env := (x, e_rhs) :: !env;
          e_rhs
        )
      else
        Ast.Assign (Ast.Var x, step e_rhs)

  (* Variable: look up its value in the environment *)
  | Ast.Var x -> lookup_env x

  (* Block: a sequence of expressions/statements *)
  | Ast.Block es -> eval_block es

  (* For other cases such as Print, you might add additional handling here *)
  | _ -> failwith "Unhandled case in step function"

(* Helper: step for binary operators *)
and step_bop bop v1 v2 =
  match bop, v1, v2 with
  | Ast.Add, Ast.Int a, Ast.Int b -> Ast.Int (a + b)
  | Ast.Add, Ast.Float a, Ast.Float b -> Ast.Float (a +. b)
  | Ast.Sub, Ast.Int a, Ast.Int b -> Ast.Int (a - b)
  | Ast.Sub, Ast.Float a, Ast.Float b -> Ast.Float (a -. b)
  | Ast.Mul, Ast.Int a, Ast.Int b -> Ast.Int (a * b)
  | Ast.Mul, Ast.Float a, Ast.Float b -> Ast.Float (a *. b)
  | Ast.Div, Ast.Int a, Ast.Int b when b <> 0 -> Ast.Int (a / b)
  | Ast.Div, Ast.Float a, Ast.Float b when b <> 0.0 -> Ast.Float (a /. b)
  | Ast.Mod, Ast.Int a, Ast.Int b when b <> 0 -> Ast.Int (a mod b)
  | Ast.Eq, Ast.Int a, Ast.Int b -> Ast.Bool (a = b)
  | Ast.Eq, Ast.Float a, Ast.Float b -> Ast.Bool (a = b)
  | Ast.Lt, Ast.Int a, Ast.Int b -> Ast.Bool (a < b)
  | Ast.Lt, Ast.Float a, Ast.Float b -> Ast.Bool (a < b)
  | Ast.Gt, Ast.Int a, Ast.Int b -> Ast.Bool (a > b)
  | Ast.Gt, Ast.Float a, Ast.Float b -> Ast.Bool (a > b)
  | Ast.Le, Ast.Int a, Ast.Int b -> Ast.Bool (a <= b)
  | Ast.Le, Ast.Float a, Ast.Float b -> Ast.Bool (a <= b)
  | Ast.Ge, Ast.Int a, Ast.Int b -> Ast.Bool (a >= b)
  | Ast.Ge, Ast.Float a, Ast.Float b -> Ast.Bool (a >= b)
  | _ -> failwith "Invalid operation or mismatched types"

(* Evaluate a block of expressions sequentially.
   Each expression is evaluated for its side effect (like assignment) and the value of the last expression is returned. *)
and eval_block (es : Ast.expr list) : Ast.expr =
  match es with
  | [] -> failwith "Empty block"
  | [e] -> eval e
  | e :: rest ->
      let _ = eval e in
      eval_block rest

(* Full evaluation: repeatedly apply step until a value is reached. *)
and eval (e : Ast.expr) : Ast.expr =
  match e with
  | Ast.Block _ -> eval_block (match e with Ast.Block es -> es | _ -> [])
  | _ ->
      if is_val e then e
      else eval (step e)

(* REPL: parse input, evaluate, and print the result *)
let rec repl () =
  try
    let input = read_line () in
    if input = "exit" then ()  (* exit the REPL *)
    else (
      let lexbuf = Lexing.from_string input in
      let ast = Parser.prog Lexer.read lexbuf in
      Printf.printf "Parsed AST: %s\n" (string_of_expr ast);
      let result = eval ast in
      Printf.printf "Evaluated: %s\n" (string_of_expr result);
      repl ()
    )
  with
  | End_of_file -> ()
  | Parsing.Parse_error -> Printf.printf "Syntax error\n"; repl ()
  | Failure msg -> Printf.printf "Error: %s\n" msg; repl ()

(* Start the REPL *)
let () = repl ()

