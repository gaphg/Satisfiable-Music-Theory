open Bachend

let s_expr_of (symbols : string list) =
  match symbols with
  | [] -> ""
  | s :: [] -> s
  | symbols -> "(" ^ (String.concat " " symbols) ^ ")"

(* expects v to only have values that are predicates *)
let rec smt_of_predicate (v : value) : string =
  match v with
  | Voice v -> string_of_int v
  | Pitch p -> 
    let bv_decimal = "bv" ^ (string_of_int p) in
    s_expr_of ["_"; bv_decimal; "7"]
  | Boolean b -> if b then "true" else "false"
  | SymbolicPitch (v, t) -> "v" ^ (string_of_int v) ^ "t" ^ (string_of_int t)
  | Equals (v1, v2) -> s_expr_of ["="; smt_of_predicate v1; smt_of_predicate v2]
  | _ -> raise (Failure "smt_of_predicate: not yet implemented or impossible")