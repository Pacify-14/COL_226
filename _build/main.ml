(* Assume we open or reference Ast so that our AST constructors (Int, Float, Binop, Var, Assign, Block, Print, etc.) are in scope *)
open Lexing
open Ast

(* Assuming Ast module is available and its constructors are as given *)
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
(* Small-step evaluation: perform one reduction step. *)
let rec is_val : Ast.expr -> bool = function
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ -> true
  | Ast.Paren e -> is_val e  (* A parenthesized expression is a value if its content is a value *)
  | _ -> false

let rec step (e : Ast.expr) : Ast.expr =
  match e with
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ ->
      failwith "Does not step further"
  | Ast.Paren e ->
    if is_val e then e
    else Ast.Paren (step e)
  | Ast.Binop (bop, e1, e2) ->
      if is_val e1 && is_val e2 then step_bop bop e1 e2
      else if is_val e1 then Ast.Binop (bop, e1, step e2)
      else Ast.Binop (bop, step e1, e2)

  | Ast.Assign (Ast.Var x, e_rhs) ->
      if is_val e_rhs then (
        env := (x, e_rhs) :: !env;
        e_rhs
      ) else
        Ast.Assign (Ast.Var x, step e_rhs)

  (*| Ast.Var x ->
      lookup_env x *)
          | Ast.Var x ->
    (match List.assoc_opt x !env with
    | Some v -> v (* Replace variable with its assigned value *)
    | None -> failwith ("Unbound variable: " ^ x))

| Ast.IfElse (cond, e1, e2) ->
    Printf.printf "Checking IfElse condition: %s\n" (string_of_expr cond);
    
    (* Helper function to unwrap parentheses *)
    let rec unwrap_paren e =
      match e with
      | Ast.Paren inner -> unwrap_paren inner
      | _ -> e
    in
    
    (* First try to unwrap any parentheses in the condition *)
    let unwrapped_cond = unwrap_paren cond in
    
    (* Then evaluate the unwrapped condition *)
    let cond_value = 
      match unwrapped_cond with
      | Ast.Var x ->
          (match List.assoc_opt x !env with
          | Some v -> v
          | None -> failwith ("Unbound variable in condition: " ^ x))
      | _ -> eval unwrapped_cond
    in
    
    Printf.printf "Condition evaluated to: %s\n" (string_of_expr cond_value);
    
    match cond_value with
    | Ast.Bool true ->
        Printf.printf "Condition is TRUE, evaluating THEN branch: %s\n" (string_of_expr e1);
        e1
    | Ast.Bool false ->
        Printf.printf "Condition is FALSE, evaluating ELSE branch: %s\n" (string_of_expr e2);
        e2
    | _ -> failwith "Condition in IfElse did not evaluate to a boolean"
  
          (*    | Ast.IfElse (cond, e1, e2) ->
      if is_val cond then
        (match cond with
         | Ast.Bool true -> e1
         | Ast.Bool false -> e2
         | _ -> failwith "Condition in IfElse is not boolean")
      else
        Ast.IfElse (step cond, e1, e2)  
*)
 (*       | Ast.IfElse (cond, e1, e2) ->
    Printf.printf "Checking IfElse condition: %s\n" (string_of_expr cond);
    if is_val cond then (
      Printf.printf "Condition is a value: %s\n" (string_of_expr cond);
      match cond with
      | Ast.Bool true ->
          Printf.printf "Condition is TRUE, evaluating THEN branch: %s\n" (string_of_expr e1);
          e1
      | Ast.Bool false ->
          Printf.printf "Condition is FALSE, evaluating ELSE branch: %s\n" (string_of_expr e2);
          e2
      | _ -> failwith "Condition in IfElse is not boolean"
    ) else (
      Printf.printf "Condition is not a value, stepping...\n";
      Ast.IfElse (step cond, e1, e2)
    )

  | Ast.Block es ->
      eval_block es

  | Ast.Print e ->
      if not (is_val e) then
        Ast.Print (step e)
      else
        (print_endline (string_of_val e); e)
*)
  | Ast.WhileLoop (cond, body) ->
      if not (is_val cond) then
        Ast.WhileLoop (step cond, body)
      else
        (match cond with
         | Ast.Bool true ->
             let _ = eval_block body in
             Ast.WhileLoop (cond, body)
         | Ast.Bool false -> Ast.Block []  (* Completed while loop returns an empty block *)
         | _ -> failwith "Condition in while loop is not boolean")

  | Ast.ForLoop (_, _, _, _) ->
      failwith "For loop not implemented"

  | _ ->
      Printf.printf "Unhandled expression type: %s\n" (string_of_expr e);
      failwith "Unhandled case in step function"

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


and step_boolop boolop v1 v2 = 
  match boolop, v1 ,v2 with
  | Ast.Or, Ast.Bool a, Ast.Bool b  -> Ast.Bool (a || b)
  | Ast.Not, Ast.Bool a,Bool b -> Ast.Bool(not a)
  | Ast.And, Ast.Bool a, Ast.Bool b -> Ast.Bool (a && b)
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

