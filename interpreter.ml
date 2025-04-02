(* Type system for the language *)
type typ =
  | IntType
  | FloatType
  | StringType
  | BoolType
  | VectorType of int
  | MatrixType of int * int
  | UnitType (* For commands that do not return a value *)

exception TypeError of string
exception RuntimeError of string

(* Type environment maps variables to their types *)
type type_env = (string * typ) list
let rec type_of_expr (expr: Ast.expr) (env: type_env) : typ =
  match expr with
  | Int _ -> IntType
  | Float _ -> FloatType
  | String _ -> StringType
  | Bool _ -> BoolType
  | Var x -> (try List.assoc x env with Not_found -> raise (TypeError ("Undefined variable: " ^ x)))

  | Binop (op, e1, e2) -> (
      match type_of_expr e1 env, type_of_expr e2 env with
      | IntType, IntType -> IntType
      | FloatType, FloatType -> FloatType
      | IntType, FloatType | FloatType, IntType -> FloatType
      | _ -> raise (TypeError "Invalid operand types for arithmetic operation")
    )

  | Boolop (op, e1, e2) -> (
      match op, type_of_expr e1 env, type_of_expr e2 env with
      | Not, BoolType, _ -> BoolType
      | (And | Or), BoolType, BoolType -> BoolType
      | _ -> raise (TypeError "Boolean operations require boolean operands")
    )

  | IfElse (cond, e1, e2) -> (
      match type_of_expr cond env with
      | BoolType ->
          let t1 = type_of_expr e1 env in
          let t2 = type_of_expr e2 env in
          if t1 = t2 then t1
          else raise (TypeError "Mismatched types in if-else branches")
      | _ -> raise (TypeError "Condition in if-else must be boolean")
    )

  | Assign (Var x, e) ->
      let t = type_of_expr e env in
      (x, t) :: env; UnitType

  | Vector elems ->
      let elem_types = List.map (fun e -> type_of_expr e env) elems in
      if List.for_all ((=) FloatType) elem_types then
        VectorType (List.length elems)
      else
        raise (TypeError "Vectors must contain only floats")

  | Matrix rows ->
      let row_types = List.map (fun row -> type_of_expr (Vector row) env) rows in
      (match row_types with
      | (VectorType n) :: _ when List.for_all ((=) (VectorType n)) row_types -> MatrixType (List.length rows, n)
      | _ -> raise (TypeError "Matrix rows must be vectors of the same size"))

  | Transpose e -> (
      match type_of_expr e env with
      | MatrixType (m, n) -> MatrixType (n, m)
      | _ -> raise (TypeError "Transpose requires a matrix")
    )

  | Det e -> (
      match type_of_expr e env with
      | MatrixType (n, n) -> FloatType
      | _ -> raise (TypeError "Determinant requires a square matrix")
    )

  | _ -> raise (TypeError "Unsupported expression in type_of_expr")
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VVector of float list
  | VMatrix of float list list
  | VString of string
  | VUnit

type env = (string * value) list

let rec eval_expr (expr: Ast.expr) (env: env) : value =
  match expr with
  | Int n -> VInt n
  | Float f -> VFloat f
  | String s -> VString s
  | Bool b -> VBool b
  | Var x -> (try List.assoc x env with Not_found -> raise (RuntimeError ("Undefined variable: " ^ x)))

  | Binop (op, e1, e2) -> (
      match eval_expr e1 env, eval_expr e2 env with
      | VInt n1, VInt n2 ->
          VInt (match op with Add -> n1 + n2 | Sub -> n1 - n2 | Mul -> n1 * n2 | Div -> n1 / n2 | Mod -> n1 mod n2 | _ -> raise (RuntimeError "Invalid int operation"))
      | VFloat f1, VFloat f2 ->
          VFloat (match op with Add -> f1 +. f2 | Sub -> f1 -. f2 | Mul -> f1 *. f2 | Div -> f1 /. f2 | _ -> raise (RuntimeError "Invalid float operation"))
      | _ -> raise (RuntimeError "Invalid operand types for arithmetic operation")
    )

  | Boolop (op, e1, e2) -> (
      match eval_expr e1 env, eval_expr e2 env with
      | VBool b1, VBool b2 -> VBool (match op with And -> b1 && b2 | Or -> b1 || b2 | _ -> raise (RuntimeError "Invalid boolean operation"))
      | _ -> raise (RuntimeError "Boolean operations require boolean operands")
    )

  | Assign (Var x, e) ->
      let v = eval_expr e env in
      (x, v) :: env; VUnit

  | Print e ->
      let v = eval_expr e env in
      (match v with
      | VInt n -> print_int n
      | VFloat f -> print_float f
      | VString s -> print_string s
      | VBool b -> print_endline (string_of_bool b)
      | VVector v -> print_endline ("[" ^ String.concat ", " (List.map string_of_float v) ^ "]")
      | VMatrix m -> List.iter (fun row -> print_endline ("[" ^ String.concat ", " (List.map string_of_float row) ^ "]")) m
      | VUnit -> ()
      ); VUnit

  | _ -> raise (RuntimeError "Unsupported expression in eval_expr")

