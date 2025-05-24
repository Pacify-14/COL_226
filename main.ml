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

let add_elements e1 e2 =
  match e1, e2 with
  | Ast.Int a, Ast.Int b -> Ast.Int (a + b)
  | Ast.Float a, Ast.Float b -> Ast.Float (a +. b)
  | Ast.Int a, Ast.Float b -> Ast.Float (float_of_int a +. b)
  | Ast.Float a, Ast.Int b -> Ast.Float (a +. float_of_int b)
  | _ -> failwith "Addition of non-numeric elements"

let is_square_matrix rows =
  let n = List.length rows in
  n > 0 && List.for_all (fun row -> List.length row = n) rows

let rec calculate_determinant matrix =
  match matrix with
  | [] -> 0.0
  | [[x]] -> x  (* 1x1 case *)
  | [[a; b]; [c; d]] -> a *. d -. b *. c  (* 2x2 optimization *)
  | _ ->
      let first_row = List.hd matrix in
      List.mapi (fun col x ->
        let sign = if col mod 2 = 0 then 1.0 else -1.0 in
        let minor =
          List.tl matrix
          |> List.map (fun row ->
              List.filteri (fun i _ -> i <> col) row)
        in
        sign *. x *. calculate_determinant minor
      ) first_row
      |> List.fold_left (+.) 0.0

let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | (x::xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let dot_product vec1 vec2 =
  try List.fold_left2 (fun acc e1 e2 ->
    match acc, e1, e2 with
    | Ast.Int a, Ast.Int b, Ast.Int c -> Ast.Int (a + b * c)
    | Ast.Float a, Ast.Int b, Ast.Int c -> Ast.Float (a +. float_of_int (b * c))
    | Ast.Int a, Ast.Float b, Ast.Float c -> Ast.Float (float_of_int a +. b *. c)
    | Ast.Float a, Ast.Float b, Ast.Float c -> Ast.Float (a +. b *. c)
    | _ -> failwith "Non-numeric elements in dot product"
  ) (Ast.Int 0) vec1 vec2
  with Invalid_argument _ -> failwith "Dot product: length mismatch"


(* Global environment: a reference to a list of variable bindings.
   Each binding maps a variable name (string) to an evaluated expression (of type Ast.expr). *)
let env : (string * Ast.expr) list ref = ref []

(* Look up a variable in the environment. *)
let lookup_env (x : string) : Ast.expr =
  try List.assoc x !env 
  with Not_found -> failwith ("Undefined variable: " ^ x)

(* Convert a final evaluated expression to a string.
   Here we assume that final values are of type Int, Float, Bool, or String. *)

let rec string_of_val (e: Ast.expr) : string =
  match e with
  | Ast.Int i -> string_of_int i
  | Ast.Float f -> string_of_float f
  | Ast.Bool b -> string_of_bool b
  | Ast.String s -> "\"" ^ s ^ "\""
  | Ast.Vector lst -> 
      "[" ^ String.concat ", " (List.map string_of_val lst) ^ "]"
  | Ast.Matrix rows ->
      "[" ^ String.concat "; " 
        (List.map (fun row -> 
          "[" ^ String.concat ", " (List.map string_of_val row) ^ "]") rows) ^ "]"
  | _ -> failwith "Not a final value"

(* A value is a fully evaluated expression (primitive types only) *)
(* Small-step evaluation: perform one reduction step. *)
let rec is_val : Ast.expr -> bool = function
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ -> true
  | Ast.Paren e -> is_val e
  | Ast.Vector elements -> List.for_all is_val elements
  | Ast.Matrix rows -> List.for_all (fun row -> List.for_all is_val row) rows
  | _ -> false

let transpose rows =
  match rows with
  | [] -> []
  | row :: _ ->
      List.mapi (fun i _ -> List.map (fun r -> List.nth r i) rows) row

let rec step (e : Ast.expr) : Ast.expr =
  match e with
  | Ast.Print e ->
      if not (is_val e) then
        Ast.Print (step e)
      else
        (print_endline (string_of_val e); e)
  | Ast.Int _ | Ast.Float _ | Ast.Bool _ | Ast.String _ ->
      failwith "Does not step further"
  | Ast.Paren e ->
    if is_val e then e
    else Ast.Paren (step e)
  | Ast.Binop (bop, e1, e2) ->
      if is_val e1 && is_val e2 then step_bop bop e1 e2
      else if is_val e1 then Ast.Binop (bop, e1, step e2)
      else Ast.Binop (bop, step e1, e2)
  | Ast.Vector elements ->
      let rec step_elements es =
        match es with
        | [] -> []
        | e :: rest ->
            if is_val e then e :: step_elements rest
            else step e :: rest
      in
      Ast.Vector (step_elements elements) 

  | Ast.Matrix rows ->
      let rec eval_row row =
        match row with
        | [] -> []
        | e :: rest ->
            if is_val e then e :: eval_row rest
            else step e :: rest
      in
      Ast.Matrix (List.map eval_row rows) 

  | Ast.Vec_ix (var, index_expr) ->
      if not (is_val index_expr) then
        Ast.Vec_ix (var, step index_expr)
      else
        let idx = match index_expr with
            | Ast.Int i -> i
            | _ -> failwith "Vector index must be an integer"
        in
        let vec = lookup_env var in
        (match vec with
        | Ast.Vector elements ->
            if idx >= 0 && idx < List.length elements then
              List.nth elements idx
            else
              failwith ("Vector index out of bounds: " ^ string_of_int idx)
        | _ -> failwith ("Variable '" ^ var ^ "' is not a vector"))
  | Ast.Boolop (boolop, e1, e2) -> 
    if is_val e1 && is_val e2 then step_boolop boolop e1 e2
    else if is_val e1 then Ast.Boolop (boolop, e1, step e2)
    else Ast.Boolop (boolop, step e1, e2)
  | Ast.Transpose e ->
      if not (is_val e) then Ast.Transpose (step e)
      else (
        match e with
        | Ast.Matrix rows -> Ast.Matrix (transpose rows)
        | _ -> failwith "Transpose requires a matrix")
        | Ast.Dim1 e ->
      if not (is_val e) then Ast.Dim1 (step e)
      else (
        match e with
        | Ast.Matrix rows -> Ast.Int (List.length rows)
        | _ -> failwith "Dim1 requires a matrix")

  | Ast.Dim2 e ->
      if not (is_val e) then Ast.Dim2 (step e)
      else (
        match e with
        | Ast.Matrix [] -> Ast.Int 0
        | Ast.Matrix (row :: _) -> Ast.Int (List.length row)
        | _ -> failwith "Dim2 requires a matrix")

  (* Matrix determinant (2x2 only) *)
| Ast.Det e ->
    if not (is_val e) then Ast.Det (step e)
    else (
      match e with
      | Ast.Vector _ -> vector_magnitude e
      | Ast.Matrix rows ->
          if not (is_square_matrix rows) then
            failwith "Invalid dimensions: matrix must be square"
          else
            let matrix, all_ints =
              List.fold_right (fun row (m_acc, int_acc) ->
                let conv_row, row_ints =
                  List.fold_right (fun elem (r_acc, ri_acc) ->
                    match elem with
                    | Ast.Int i -> (float_of_int i :: r_acc, ri_acc)
                    | Ast.Float f -> (f :: r_acc, false)
                    | _ -> failwith "Matrix contains non-numeric elements"
                  ) row ([], true)
                in
                (conv_row :: m_acc, int_acc && row_ints)
              ) rows ([], true)
            in
            let det = calculate_determinant matrix in
            if all_ints && det = floor det then
              Ast.Int (int_of_float det)
            else
              Ast.Float det
      | _ -> failwith "det_mat requires vector or matrix"
    )  
  | Ast.VecDim e ->
      if not (is_val e) then Ast.VecDim (step e)
      else (
        match e with
        | Ast.Vector vec -> Ast.Int (List.length vec)
        | _ -> failwith "dim_vec requires a vector"
      )
  | Ast.Assign (Ast.Var x, e_rhs) ->
      if is_val e_rhs then (
        env := (x, e_rhs) :: !env;
        e_rhs
      ) else
        Ast.Assign (Ast.Var x, step e_rhs)

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
    
    let unwrapped_cond = unwrap_paren cond in

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


(* Updated step_bop for multiplication *)
(* Updated scalar multiplication section *)
and step_bop bop v1 v2 =
  match bop, v1, v2 with
  (* ------ Scalar Operations First ------ *)
  (* Addition *)
  | Ast.Add, Ast.Int a, Ast.Int b -> Ast.Int (a + b)
  | Ast.Add, Ast.Float a, Ast.Float b -> Ast.Float (a +. b)
  | Ast.Add, Ast.Int a, Ast.Float b -> Ast.Float (float_of_int a +. b)
  | Ast.Add, Ast.Float a, Ast.Int b -> Ast.Float (a +. float_of_int b)

  (* Subtraction *)
  | Ast.Sub, Ast.Int a, Ast.Int b -> Ast.Int (a - b)
  | Ast.Sub, Ast.Float a, Ast.Float b -> Ast.Float (a -. b)
  | Ast.Sub, Ast.Int a, Ast.Float b -> Ast.Float (float_of_int a -. b)
  | Ast.Sub, Ast.Float a, Ast.Int b -> Ast.Float (a -. float_of_int b)

  (* Multiplication - Scalars First *)
  | Ast.Mul, Ast.Int a, Ast.Int b -> Ast.Int (a * b)
  | Ast.Mul, Ast.Float a, Ast.Float b -> Ast.Float (a *. b)
  | Ast.Mul, Ast.Int a, Ast.Float b -> Ast.Float (float_of_int a *. b)
  | Ast.Mul, Ast.Float a, Ast.Int b -> Ast.Float (a *. float_of_int b)

  (* Division *)
  | Ast.Div, Ast.Int a, Ast.Int b when b <> 0 -> Ast.Int (a / b)
  | Ast.Div, Ast.Float a, Ast.Float b when b <> 0.0 -> Ast.Float (a /. b)
  | Ast.Div, Ast.Int a, Ast.Float b when b <> 0.0 -> Ast.Float (float_of_int a /. b)
  | Ast.Div, Ast.Float a, Ast.Int b when b <> 0 -> Ast.Float (a /. float_of_int b)

  (* Modulo *)
  | Ast.Mod, Ast.Int a, Ast.Int b when b <> 0 -> Ast.Int (a mod b)
| Ast.Lt, Ast.Vector v1, Ast.Vector v2 ->
    if List.length v1 <> List.length v2 then
      failwith "Vectors must have same dimension for angle calculation"
    else
      let vec1 = Ast.Vector v1 in  (* Create proper Vector expr *)
      let vec2 = Ast.Vector v2 in  (* Create proper Vector expr *)
      let dot = match dot_product v1 v2 with
        | Ast.Int i -> float_of_int i
        | Ast.Float f -> f
        | _ -> failwith "Non-numeric dot product"
      in
      let mag1 = match vector_magnitude vec1 with
        | Ast.Float f -> f
        | _ -> failwith "Invalid magnitude calculation"
      in
      let mag2 = match vector_magnitude vec2 with
        | Ast.Float f -> f
        | _ -> failwith "Invalid magnitude calculation"
      in
      if mag1 = 0.0 || mag2 = 0.0 then
        failwith "Cannot calculate angle with zero vector"
      else
        let cos_theta = dot /. (mag1 *. mag2) in
        Ast.Float (acos cos_theta)
  (* Comparison Operators *)
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

  (* ------ Vector/Matrix Operations ------ *)
  (* Vector addition *)
  | Ast.Add, Ast.Vector lst1, Ast.Vector lst2 ->
      (try Ast.Vector (List.map2 add_elements lst1 lst2)
       with Invalid_argument _ -> failwith "Vector addition: length mismatch")

  (* Matrix addition *)
  | Ast.Add, Ast.Matrix rows1, Ast.Matrix rows2 ->
      (try Ast.Matrix (List.map2 (fun r1 r2 -> List.map2 add_elements r1 r2) rows1 rows2)
       with Invalid_argument _ -> failwith "Matrix addition: dimension mismatch")

  (* Scalar * Vector *)
  | Ast.Mul, (Ast.Int _ | Ast.Float _ as scalar), Ast.Vector vec ->
      Ast.Vector (List.map (multiply_element scalar) vec)

  (* Scalar * Matrix *)
  | Ast.Mul, (Ast.Int _ | Ast.Float _ as scalar), Ast.Matrix rows ->
      Ast.Matrix (List.map (List.map (multiply_element scalar)) rows)

  (* Vector * Scalar (commutative) *)
  | Ast.Mul, Ast.Vector vec, (Ast.Int _ | Ast.Float _ as scalar) ->
      step_bop Ast.Mul scalar (Ast.Vector vec)

  (* Matrix * Scalar (commutative) *)
  | Ast.Mul, Ast.Matrix rows, (Ast.Int _ | Ast.Float _ as scalar) ->
      step_bop Ast.Mul scalar (Ast.Matrix rows)

  (* Vector dot product *)
  | Ast.Mul, Ast.Vector vec1, Ast.Vector vec2 ->
      dot_product vec1 vec2

  (* Matrix multiplication *)
  | Ast.Mul, Ast.Matrix m1, Ast.Matrix m2 ->
      let cols_m1 = List.length (List.hd m1) in
      let rows_m2 = List.length m2 in
      if cols_m1 <> rows_m2 then
        failwith "Matrix multiplication: cols(m1) != rows(m2)"
      else
        let m2_t = transpose m2 in
        Ast.Matrix (
          List.map (fun row ->
            List.map (fun col -> dot_product row col) m2_t
          ) m1
        )

  (* Matrix * Vector *)
  | Ast.Mul, Ast.Matrix m, Ast.Vector v ->
      if List.length (List.hd m) <> List.length v then
        failwith "Matrix columns != Vector length"
      else
        Ast.Vector (List.map (fun row -> dot_product row v) m)

  (* Vector * Matrix *)
  | Ast.Mul, Ast.Vector v, Ast.Matrix m ->
      if List.length v <> List.length m then
        failwith "Vector length != Matrix rows"
      else
        let m_t = transpose m in
        Ast.Vector (List.map (fun col -> dot_product v col) m_t)

  (* ------ Fallthrough Error ------ *)
  | _ -> failwith "Invalid operation or mismatched types"

and multiply_element scalar e =
  match scalar, e with
  | Ast.Int a, Ast.Int b -> Ast.Int (a * b)
  | Ast.Float a, Ast.Int b -> Ast.Float (a *. float_of_int b)
  | Ast.Int a, Ast.Float b -> Ast.Float (float_of_int a *. b)
  | Ast.Float a, Ast.Float b -> Ast.Float (a *. b)
  | _ -> failwith "Non-numeric multiplication"

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
and vector_magnitude vec_expr =
  match vec_expr with
  | Ast.Vector vec ->
      let squares = List.map (function
        | Ast.Int i -> float_of_int (i * i)
        | Ast.Float f -> f *. f
        | _ -> failwith "Vector magnitude requires numeric elements"
      ) vec in
      Ast.Float (sqrt (List.fold_left (+.) 0.0 squares))
  | _ -> failwith "Magnitude requires a vector"

(* Update Det case to handle vectors *)

(* REPL: parse input, evaluate, and print the result *)
let rec repl () =
  try
    let input = read_line () in
    (* Skip empty lines and comments *)
    if String.trim input = "" || String.length input = 0 then repl ()
    else if input = "exit" then ()  (* Optional exit command *)
    else (
      let lexbuf = Lexing.from_string input in
      try
        let ast = Parser.prog Lexer.read lexbuf in
        Printf.printf "Parsed AST: %s\n" (string_of_expr ast);
        let result = eval ast in
        Printf.printf "Result: %s\n" (string_of_val result);
      with
      | Parsing.Parse_error -> Printf.printf "Syntax error in: %s\n" input
      | Failure msg -> Printf.printf "Error: %s\n" msg
    );
    repl ()  (* Process next line *)
  with
  | End_of_file -> ()  (* Exit on EOF for file input *)
  | exn ->
      Printf.printf "Unexpected error: %s\n" (Printexc.to_string exn);
      repl ()

(* Start the REPL without initial prompt *)
let () = repl ()
