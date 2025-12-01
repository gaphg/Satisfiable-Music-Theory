open Bachend

let s_expr_of (symbols : string list) =
  match symbols with
  | [] -> ""
  | s :: [] -> s
  | symbols -> "(" ^ (String.concat " " symbols) ^ ")"


let const_name_of_voice_time v t =
 "v" ^ (string_of_int v) ^ "t" ^ (string_of_int t)

let bv_decimal (num : int) =
  if num >= 0
    then s_expr_of ["_"; "bv" ^ (string_of_int num); "8"]
else s_expr_of["bvneg"; s_expr_of ["_"; "bv" ^ (string_of_int (-num)); "8"]]

let initialize_smt (env : dynamic_environment) : string list =
  match env.voice_count, env.song_length_units with
  | Some voice_count, Some song_length_units -> 
    "(set-logic QF_BV)" ::
    "(define-fun bvabs ((x (_ BitVec 8))) (_ BitVec 8) (ite (bvslt x (_ bv0 8)) (bvneg x) x))" ::
    List.init 
      (voice_count * song_length_units) 
      (fun n ->
        let v = n / song_length_units in
        let t = n mod song_length_units in
        let const_name = const_name_of_voice_time v t in
        s_expr_of ["declare-const"; const_name; "(_ BitVec 8)"])
      
  | _ -> raise (Failure "declare_symbols: not yet implemented")

(* expects v to only have values that are predicates *)
let rec smt_of_predicate (v : value) : string =
  match v with
  (* | Voice v -> string_of_int v *)
  | Pitch p -> bv_decimal p
  | Interval (i, b) -> begin
    match b with
    | None | Some true -> bv_decimal i
    | Some false -> bv_decimal (-i)
  end
  | Boolean b -> if b then "true" else "false"
  | SymbolicPitch (v, t) -> const_name_of_voice_time v t
  | SymbolicInterval (v1, v2) -> s_expr_of ["bvsub"; smt_of_predicate v2; smt_of_predicate v1]
  | SymbolicEquals (v1, v2) -> s_expr_of ["="; smt_of_predicate v1; smt_of_predicate v2]
  | SymbolicAbs v -> s_expr_of["bvabs"; smt_of_predicate v]
  | SymbolicOr l -> s_expr_of ("or" :: List.map smt_of_predicate l)
  | _ -> raise (Failure ("smt_of_predicate: not yet implemented or impossible " ^ (show_value v)))