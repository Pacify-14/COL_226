(* Module: Terms, Signatures, Substitutions, and Unification in OCaml *)

module Term = struct
  (* Variable and Symbol types *)
  type variable = string
  type symbol   = string * int    (* name and arity stored, but signature defines true arities *)

  (* Pre-term and well-formed term representation using arrays *)
  type preterm =
    | V of variable
    | Node of symbol * preterm array

  (* A signature is a list of symbols with arities *)
  type signature = symbol list

  (* Check that a signature is valid: no duplicate names, non-negative arities *)
  let check_sig (sig_ : signature) : bool =
    let names = Hashtbl.create (List.length sig_) in
    List.for_all (fun (s, ar) ->
        ar >= 0 &&
        if Hashtbl.mem names s then false
        else ( Hashtbl.add names s (); true)
      ) sig_

  (* Lookup arity in signature *)
  let arity_of (sig_ : signature) (name : string) : int =
    try List.assoc name sig_ with Not_found -> -1

  (* Well-formedness: each Node's symbol occurs in signature and has correct number of children *)
  let rec wfterm (sig_ : signature) (t : preterm) : bool = match t with
    | V _ -> true
    | Node ((s, _), childs) ->
        let expected = arity_of sig_ s in
        expected >= 0 && Array.length childs = expected &&
        Array.for_all (wfterm sig_) childs

  (* Height of a term *)
  let rec ht t = match t with
    | V _ -> 0
    | Node (_, childs) ->
        if Array.length childs = 0 then 0
        else 1 + Array.fold_left (fun acc t' -> max acc (ht t')) 0 childs

  (* Size (# of nodes) of a term *)
  let rec size t = match t with
    | V _ -> 1
    | Node (_, childs) ->
        1 + Array.fold_left (fun acc t' -> acc + size t') 0 childs

  (* Variables appearing in a term: use String Set *)
  module VS = Set.Make(String)
  let rec vars t = match t with
    | V x -> VS.singleton x
    | Node (_, childs) ->
        Array.fold_left (fun acc t' -> VS.union acc (vars t')) VS.empty childs

end

module Subst = struct
  open Term
  (* A substitution is a mapping from variable to term, implemented as an assoc list for finite domain *)
  type t = (variable * preterm) list

  let empty : t = []
  let single x u : t = [(x, u)]

  (* Lookup or identity *)
  let rec apply_subst (s : t) (x : variable) : preterm = match s with
    | (y, u)::rest -> if x = y then u else apply_subst rest x
    | [] -> V x

  (* Homomorphic extension: substitute throughout the term *)
  let rec subst (s : t) (t0 : preterm) : preterm = match t0 with
    | V x -> apply_subst s x
    | Node (sym, childs) ->
        let childs' = Array.map (subst s) childs in
        Node (sym, childs')

  (* Compose substitutions: s2 after s1 *)
  let compose (s1 : t) (s2 : t) : t =
    (* apply s2 to images of s1, then include s2's own bindings not in s1 *)
    let mapped = List.map (fun (x, u) -> (x, subst s2 u)) s1 in
    mapped @ List.filter (fun (y, _) -> not (List.exists (fun (x, _) -> x = y) s1)) s2

end

module Unify = struct
  open Term
  open Subst
  exception NOT_UNIFIABLE

  (* Occurs-check: does variable x occur in term t? *)
  let rec occurs (x : variable) (t : preterm) : bool = match t with
    | V y -> x = y
    | Node (_, childs) -> Array.exists (occurs x) childs

  (* Unify two terms, returning an mgu as a substitution *)
  let mgu (t1 : preterm) (t2 : preterm) : t =
    let rec unify_pairs (subs : t) (pairs : (preterm * preterm) list) : t = match pairs with
      | [] -> subs
      | (u, v)::rest ->
          let u' = subst subs u in
          let v' = subst subs v in
          match (u', v') with
          | V x, V y when x = y -> unify_pairs subs rest
          | V x, _ -> if occurs x v' then raise NOT_UNIFIABLE else
                let s0 = single x v' in
                let subs' = compose subs s0 in
                unify_pairs subs' rest
          | _, V y -> unify_pairs subs ((V y, u')::rest)
          | Node ((f, ar1), c1), Node ((g, ar2), c2)
            when f = g && ar1 = ar2 ->
              let new_pairs = Array.to_list (Array.map2 (fun a b -> (a, b)) c1 c2) in
              unify_pairs subs (new_pairs @ rest)
          | _ -> raise NOT_UNIFIABLE
    in
    unify_pairs empty [(t1, t2)]

end

module Edit = struct
  open Term
  open Subst
  (* Positions as list of indices *)
  type pos = int list

  (* Functional replace: returns new term *)
  let rec edit (t : preterm) (pos : pos) (replacement : preterm) : preterm = match pos with
    | [] -> replacement
    | i::rest -> begin match t with
        | V _ -> failwith "Invalid position"
        | Node (sym, childs) ->
            if i < 0 || i >= Array.length childs then failwith "Index out of bounds"
            else
              let new_child = edit childs.(i) rest replacement in
              let new_arr = Array.copy childs in
              new_arr.(i) <- new_child;
              Node (sym, new_arr)
      end

  (* In-place substitution stub: needs mutable structure to work *)
  let rec subst_inplace (s : t) (t : preterm) : unit = match t with
    | V x -> (match apply_subst s x with
        | V y when y = x -> ()
        | _ -> failwith "In-place subst requires mutable nodes"
      )
    | Node (_, childs) -> Array.iter (subst_inplace s) childs

end

open Term

(* ------------------------------------------------------------------ *)
(* TEST HELPERS                                                       *)
(* ------------------------------------------------------------------ *)
let test_case name f =
  Printf.printf "Running test: %s\n" name;
  try
    f ();
    Printf.printf "  [✓] Passed\n"
  with e ->
    Printf.printf "  [✗] Failed: %s\n" (Printexc.to_string e)

(* Test data setup *)
let sig_valid = [("f", 2); ("g", 1); ("h", 0); ("i", 3)]

(* Define substitutions for tests *)
let sub1 = [("x", Node(("h",0),[||]))]
let sub2 = [("y", Node(("h",0),[||]))]
let comp = Subst.compose sub1 sub2

(* ------------------------------------------------------------------ *)
(* RUN ALL TESTS                                                     *)
(* ------------------------------------------------------------------ *)
let () =
  (* ---------- SIGNATURE TESTS ---------- *)
  test_case "check_sig sig_valid" (fun () ->
      assert (check_sig sig_valid)
    );
  test_case "check_sig sig_invalid_arity" (fun () ->
      let sig_invalid_arity = [("f", -1); ("g", 2)] in
      assert (not (check_sig sig_invalid_arity))
    );
  test_case "check_sig sig_invalid_duplicate" (fun () ->
      let sig_invalid_duplicate = [("f",  2); ("f", 3)] in
      assert (not (check_sig sig_invalid_duplicate))
    );
  test_case "check_sig sig_empty" (fun () ->
      assert (check_sig [])
    );

  (* ---------- WELL-FORMED TERM TESTS ---------- *)
  test_case "wfterm t_valid1" (fun () ->
      let t_valid1 = Node(("f",2), [| V "x"; Node(("g",1), [| V "y" |]) |]) in
      assert (wfterm sig_valid t_valid1)
    );
  test_case "wfterm t_valid2" (fun () ->
      let t_valid2 = Node(("i",3), [| Node(("h",0), [||]); V "z"; Node(("h",0), [||]) |]) in
      assert (wfterm sig_valid t_valid2)
    );
  test_case "wfterm t_invalid1" (fun () ->
      let t_invalid1 = Node(("f",2), [| V "x" |]) in
      assert (not (wfterm sig_valid t_invalid1))
    );
  test_case "wfterm t_invalid2" (fun () ->
      let t_invalid2 = Node(("unknown",1), [| V "x" |]) in
      assert (not (wfterm sig_valid t_invalid2))
    );

  (* ---------- TERM-PROPERTY TESTS ---------- *)
  let t_complex = Node(("f",2), [|
      Node(("g",1), [| Node(("g",1), [| V "x" |]) |]);
      Node(("g",1), [| Node(("g",1), [| Node(("g",1), [| V "y" |]) |]) |])
    |]) in
  test_case "ht t_complex" (fun () ->
      assert (ht t_complex = 4)
    );
  test_case "size t_complex" (fun () ->
      assert (size t_complex = 8)
    );
  test_case "vars t_complex" (fun () -> 
  let vs = VS.elements (vars t_complex) in
  assert (vs = ["x"; "y"])
);
  test_case "subst sub1 t_complex" (fun () ->
      let t' = Subst.subst sub1 t_complex in
    (* just check one spot *)
      match t' with
      | Node(_, [| Node(_, [| Node(_, [| Node(("h",0),[||]) |]) |]); _ |]) -> ()
      | _ -> assert false
    );
  
  (* ---------- SUBSTITUTION COMPOSITION TESTS ---------- *)
  test_case "compose sub1 sub2 on x" (fun () ->
      match Subst.subst comp (V "x") with Node(("h",0),[||]) -> () | _ -> assert false
    );
  test_case "compose sub1 sub2 on y" (fun () ->
      match Subst.subst comp (V "y") with Node(("h",0),[||]) -> () | _ -> assert false
    );
  test_case "compose sub1 sub2 on z" (fun () ->
      match Subst.subst comp (V "z") with V "z" -> () | _ -> assert false
    );

  (* ---------- MGU TESTS ---------- *)
  test_case "mgu trivial unification" (fun () ->
      let t1, t2 = V "x", V "x" in
      let u = Unify.mgu t1 t2 in
      assert (Subst.subst u t1 = Subst.subst u t2)
    );
  test_case "mgu unify variable with node" (fun () ->
      let t1, t2 = V "x", Node(("h",0),[||]) in
      let u = Unify.mgu t1 t2 in
      assert (Subst.subst u t1 = Subst.subst u t2)
    );
  test_case "mgu failure due to occurs check" (fun () ->
      let t1, t2 = V "x", Node(("g",1),[| V "x" |]) in
      try let _ = Unify.mgu t1 t2 in assert false 
      with Unify.NOT_UNIFIABLE -> ()
    );
  test_case "mgu non-trivial unification" (fun () ->
      let t1 = Node(("f", 2), [| V "x"; Node(("h", 0), [||]) |]) in
      let t2 = Node(("f", 2), [| Node(("g", 1), [| V "z" |]); V "y" |]) in
      let u = Unify.mgu t1 t2 in
    (* check one mapping, at least *)
      assert (Subst.subst u t1 = Subst.subst u t2)
    );
  test_case "mgu failure due to symbol mismatch" (fun () ->
      let t1 = Node(("f", 2), [| V "x"; V "y" |]) in
      let t2 = Node(("g", 2), [| V "x"; V "y" |]) in
      try let _ = Unify.mgu t1 t2 in assert false 
      with Unify.NOT_UNIFIABLE -> ()
    );

  (* ---------- EDIT TEST ---------- *)
  test_case "edit test" (fun () ->
      let t = Node(("f",2), [| V "x"; V "y" |]) in
      let edited = Edit.edit t [0] (Node(("h",0),[||])) in (*[0] ki jagha [|0|] if given as per the original test case then error is caused.*)
      match edited with
      | Node(("f",2),[| Node(("h",0),[||]); V "y" |]) -> ()
      | _ -> assert false
    );

  (* ---------- IN-PLACE SUBSTITUTION TEST ---------- *)
test_case "inplace_subst test" (fun () ->
  let t = Node(("f", 2), [| V "x"; Node(("g", 1), [| V "y" |]) |]) in
  let sub_func x = if x = "x" then Node(("h", 0), [||])
                  else if x = "y" then Node(("h", 0), [||])
                  else V x in
  let vars = ["x"; "y"] in
  let sub = List.map (fun x -> (x, sub_func x)) vars in
  Edit.subst_inplace sub t;
  match t with
  | Node(("f", 2), [| Node(("h", 0), [||]); Node(("g", 1), [| Node(("h", 0), [||]) |]) |]) -> ()
  | _ -> assert false
)
