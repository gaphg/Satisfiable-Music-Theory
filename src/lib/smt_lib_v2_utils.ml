open Bachend

let s_expr_of (symbols : string list) =
  match symbols with
  | [] -> ""
  | s :: [] -> s
  | symbols -> "(" ^ String.concat " " symbols ^ ")"

let const_name_of_voice_time v t = "v" ^ string_of_int v ^ "t" ^ string_of_int t

let bv_decimal (num : int) =
  if num >= 0 then s_expr_of [ "_"; "bv" ^ string_of_int num; "8" ]
  else
    s_expr_of [ "bvneg"; s_expr_of [ "_"; "bv" ^ string_of_int (-num); "8" ] ]

let initialize_smt (env : dynamic_environment) : string list =
  match (env.voice_count, env.song_length_units) with
  | Some voice_count, Some song_length_units ->
      "(set-logic QF_BV)"
      :: "(define-fun bvabs ((x (_ BitVec 8))) (_ BitVec 8) (ite (bvslt x (_ \
          bv0 8)) (bvneg x) x))"
      :: List.init (voice_count * song_length_units) (fun n ->
             let v = n / song_length_units in
             let t = n mod song_length_units in
             let const_name = const_name_of_voice_time v t in
             s_expr_of [ "declare-const"; const_name; "(_ BitVec 8)" ])
  | _ -> raise (Failure "initialize_smt: not yet implemented")

(* expects v to only have values that are predicates *)
let rec smt_of_predicate (v : vc_term) : string =
  let bin_op name v1 v2 = s_expr_of [ name; smt_of_predicate v1; smt_of_predicate v2 ] in
  match v with
  (* | Voice v -> string_of_int v *)
  | Pitch p -> bv_decimal p
  | Interval (i, b) -> (
      match b with
      | None | Some true -> bv_decimal i
      | Some false -> bv_decimal (-i))
  | Boolean b -> if b then "true" else "false"
  | SymbolicPitch (v, t) -> const_name_of_voice_time v t
  | SymbolicInterval (v1, v2) -> bin_op "bvsub" v2 v1
  | SymbolicModOct v ->
      s_expr_of [ "bvsmod"; smt_of_predicate v; bv_decimal 12 ]
  | SymbolicEquals (v1, v2) -> bin_op "=" v1 v2
  | SymbolicLt (v1, v2) -> bin_op "bvslt" v1 v2
  | SymbolicLe (v1, v2) -> bin_op "bvsle" v1 v2
  | SymbolicGt (v1, v2) -> bin_op "bvsgt" v1 v2
  | SymbolicGe (v1, v2) -> bin_op "bvsge" v1 v2
  | SymbolicAbs v -> s_expr_of [ "bvabs"; smt_of_predicate v ]
  | SymbolicNot v -> s_expr_of [ "not"; smt_of_predicate v ]
  | SymbolicAnd l -> s_expr_of ("and" :: List.map smt_of_predicate l)
  | SymbolicOr l -> s_expr_of ("or" :: List.map smt_of_predicate l)
  | SymbolicImplies (v1, v2) -> bin_op "=>" v1 v2
  | _ ->
      raise
        (Failure
           ("smt_of_predicate: not yet implemented or impossible "
          ^ show_vc_term v))

let asserts_of_tracks (tracks : int list list) =
  tracks
  |> List.mapi (fun v track ->
         List.mapi
           (fun t pitch ->
             s_expr_of
               [
                 "assert";
                 s_expr_of
                   [ "="; const_name_of_voice_time v t; bv_decimal pitch ];
               ])
           track)
  |> List.concat

let whitespace = Re.(alt [ space; char '\n' ])

let match_model =
  Re.(
    compile
      (seq
         [
           str "(define-fun ";
           char 'v';
           group ~name:"voice_id" (rep1 digit);
           char 't';
           group ~name:"time_step" (rep1 digit);
           str " () (_ BitVec 8)";
           rep whitespace;
           str "#";
           group ~name:"pitch" (seq [ char 'x'; xdigit; xdigit ]);
           char ')';
         ]))

(* takes in a model string of the form
   (model
     (define-fun v?t? () (_ BitVec 8)
       #x??)
     ...
   )
   and returns a 2d list of pitches
*)
let parse_model (model : string) (num_voices : int) (song_length : int) :
    int list list =
  let grid = Array.make_matrix num_voices song_length 0 in
  Re.(
    let matches = all match_model model in
    matches
    |> List.iter (fun m ->
           let v = int_of_string (Group.get m 1) in
           let t = int_of_string (Group.get m 2) in
           let pitch = int_of_string ("0" ^ Group.get m 3) in
           grid.(v).(t) <- pitch));
  Array.to_list (Array.map Array.to_list grid)
