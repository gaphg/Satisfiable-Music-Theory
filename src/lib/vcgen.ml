open Bachend
open Ast
open Types
open Errors
open Type_checker
open Smt_lib_v2_utils

let possible_values_of_type (env : dynamic_environment) (t : st_type) :
    vc_term list =
  match t with
  | VoiceType -> (
      match env.voice_count with
      | Some n -> List.init n (fun i -> Voice i)
      | None ->
          raise
            (RuntimeError
               "Voice count has not been configured/no midi file input"))
  | PitchType -> (
    match env.voice_count, env.song_length_units with
    | Some voice_count, Some song_length -> 
      List.init voice_count (fun v -> 
        List.init song_length (fun t -> 
          SymbolicPitch (v, t)))
      |> List.concat
    | _ -> raise (RuntimeError "Song length or voice count has not been configured")
  )
  | TimeStepType -> (
      match env.song_length_units with
      | Some n -> List.init n (fun i -> TimeStep i)
      | None ->
          raise
            (RuntimeError
               "Song length units has not been configured/could not be \
                determined"))
  | BooleanType -> [ Boolean true; Boolean false ]
  | IntegerType -> List.init (1 lsl 8) (fun i -> Integer i)
  | _ -> raise (Failure ("possible_values_of_type: not yet implemented for type " ^ (string_of_type t)))

let rec translate_expr (env : dynamic_environment) (e : expr) : vc_term =
  (* helper functions *)
  let translate_numeric_bin_op op e1 e2 =
    match (translate_expr env e1, translate_expr env e2) with
    | TimeStep t1, TimeStep t2 -> TimeStep (op t1 t2)
    | Integer i1, Integer i2 -> Integer (op i1 i2)
    | _ -> raise (Failure "interpret_expr: not yet implemented")
  in
  (* signed mod *)
  let smod n d = ((n mod d) + d) mod d in
  (* give wiggle room to intervals, other stuff *)
  let handle_comparison e1 e2 symbolic_fun concrete_fun =
      let v1, v2 = (translate_expr env e1, translate_expr env e2) in
      (* deal with some wiggle room for equality *)
      match (v1, v2) with
      | Voice i, Voice j 
      | Pitch i, Pitch j
      | TimeStep i, TimeStep j
      | Integer i, Integer j -> Boolean (concrete_fun i j)
      | Interval (i1, Some d1), Interval (i2, Some d2) ->
          Boolean (d1 = d2 && i1 = i2)
      | Interval (_, None), _ -> symbolic_fun v1 (SymbolicAbs v2)
      | _, Interval (_, None) -> symbolic_fun (SymbolicAbs v1) v2
      | _ -> symbolic_fun v1 v2
  in
  let rec branch_quantifier env vars e =
    match vars with
    | [] -> raise (RuntimeError "translate_expr: impossible, vars should be nonempty")
    | (name, list_e) :: vars -> (
      match translate_expr env list_e with
      | StList l | TimeSeries l ->
        l |> List.map (fun var_e ->
        let v = translate_expr env var_e in
        let new_env = { env with
          venv = (name, v) :: env.venv          
        } in
        if vars = [] 
          then [translate_expr new_env e]
        else branch_quantifier new_env vars e)
        |> List.concat
      | _ -> raise (Failure "translate_expr: impossible, expected list"))
  in
  match e with
  | SymbolicPitchExpr (v, t) -> SymbolicPitch (v, t)
  | SymbolicIntervalExpr ((v1, t1), (v2, t2)) ->
      SymbolicInterval (SymbolicPitch (v1, t1), SymbolicPitch (v2, t2))
  (* literals *)
  | PitchLit p -> Pitch p
  | IntervalLit (i, d) -> Interval (i, d)
  | TimeStepLit t -> TimeStep t
  | IntegerLit i -> Integer i
  | BooleanLit b -> Boolean b
  | DirectionLit d -> Direction d
  (* variables/functions *)
  | Var name -> (
      match List.assoc_opt name env.venv with
      | Some v -> v
      | None -> raise (RuntimeError ("unbound variable " ^ name)))
  | FuncCall (fname, args) ->
      let arg_names, body = List.assoc fname env.fenv in
      let scoped_venv =
        List.combine arg_names (List.map (translate_expr env) args)
      in
      let scoped_env = { env with venv = scoped_venv @ env.venv } in
      translate_expr scoped_env body
  | ListExpr l -> StList l
  (* builtin functions on voices *)
  | Pitches v -> (
      match (translate_expr env v, env.song_length_units) with
      | Voice v, Some song_length_units ->
          TimeSeries
            (List.init song_length_units (fun t -> SymbolicPitchExpr (v, t)))
      | Voice _, None ->
          raise
            (RuntimeError
               "Song length units has not been configured/cannot be determined")
      | _ -> raise (Failure "interpret_expr: impossible"))
  | Contour v -> (
      match (translate_expr env v, env.song_length_units) with
      | Voice v, Some song_length_units ->
          TimeSeries
            (List.init (song_length_units - 1) (fun t ->
                 SymbolicIntervalExpr ((v, t), (v, t + 1))))
      | Voice _, None ->
          raise
            (RuntimeError
               "Song length units has not been configured/cannot be determined")
      | _ -> raise (Failure "interpret_expr: impossible"))
  | Diads (v1, v2) -> (
      match
        (translate_expr env v1, translate_expr env v2, env.song_length_units)
      with
      | Voice v1, Voice v2, Some song_length_units ->
          TimeSeries
            (List.init song_length_units (fun t ->
                 SymbolicIntervalExpr ((v1, t), (v2, t))))
      | Voice _, Voice _, None ->
          raise
            (RuntimeError
               "Song length units has not been configured/cannot be determined")
      | _ -> raise (Failure "interpret_expr: impossible"))
  | IntervalBetween (p1, p2) ->
      SymbolicInterval (translate_expr env p1, translate_expr env p2)
  | Plus (e1, e2) -> translate_numeric_bin_op (+) e1 e2
  | Minus (e1, e2) -> translate_numeric_bin_op (-) e1 e2
  | Mod (e1, e2) -> translate_numeric_bin_op (smod) e1 e2
  (* boolean/predicate *)
  | Not e -> SymbolicNot (translate_expr env e)
  | And (e1, e2) -> SymbolicAnd [ translate_expr env e1; translate_expr env e2 ]
  | Or (e1, e2) -> SymbolicOr [ translate_expr env e1; translate_expr env e2 ]
  | Implies (e1, e2) -> SymbolicImplies (translate_expr env e1, translate_expr env e2)
  | Iff (e1, e2) -> SymbolicEquals (translate_expr env e1, translate_expr env e2)
  | Exists (vars, e) -> SymbolicOr (branch_quantifier env vars e)
  | Forall (vars, e) -> SymbolicAnd (branch_quantifier env vars e)
  (* comparisons *)
  | Equals (e1, e2) -> 
      handle_comparison e1 e2 (fun v1 v2 -> SymbolicEquals (v1, v2)) (=)
  | NotEquals (e1, e2) -> translate_expr env (Not (Equals (e1, e2)))
  | LessThan (e1, e2) -> 
      handle_comparison e1 e2 (fun v1 v2 -> SymbolicLt (v1, v2)) (<)
  | LessThanEq (e1, e2) -> 
      handle_comparison e1 e2 (fun v1 v2 -> SymbolicLe (v1, v2)) (<=)
  | GreaterThan (e1, e2) ->
      handle_comparison e1 e2 (fun v1 v2 -> SymbolicGt (v1, v2)) (>)
  | GreaterThanEq (e1, e2) ->
      handle_comparison e1 e2 (fun v1 v2 -> SymbolicGe (v1, v2)) (>=)
  | Flatten e -> (
    let v = translate_expr env e in
    match v with
    | Pitch p -> Pitch (smod p 12)
    | Interval (i, d) -> Interval (smod i 12, d)
    | SymbolicPitch _
    | SymbolicInterval _ -> SymbolicModOct v (*TODO: verify this is correct for sym interval? *)
    | _ -> raise (Failure "interpret_expr: flatten should only have interval or pitch")
  )
  | EqualsModOctave (e1, e2) -> translate_expr env (Equals (Flatten e1, Flatten e2))
  | NotEqualsModOctave (e1, e2) -> translate_expr env (Not (EqualsModOctave (e1, e2)))
  | ElementAt (list, idx) -> (
      match (translate_expr env list, translate_expr env idx) with
      | TimeSeries l, TimeStep i | StList l, Integer i ->
          if i < 0 || List.length l <= i then (print_endline ("length: " ^ (string_of_int (List.length l)) ^ ", i: " ^ (string_of_int i));
          raise (InvalidIndexError i))
          else translate_expr env (List.nth l i)
      | _ -> raise (Failure "interpret_expr: impossible"))
  | Contains (list, elt) -> (
      match translate_expr env list with
      | StList es | TimeSeries es ->
          SymbolicOr
            (List.map (fun e -> translate_expr env (Equals (e, elt))) es)
      | _ -> raise (Failure "interpret_expr: impossible"))
  | _ -> raise (Failure "translate_expr: not yet implemented")

(* for each free variable, interprets the expression for all possible values of that variable, outputting a list of values 
TODO: decide whether to use forall smt quantifier (not sure if it's supported as well, gotta research )*)
and branch_on_free_vars (env : dynamic_environment)
    (free_vars : var_type_context) (e : expr) : vc_term list =
  match free_vars with
  | [] -> (
      (* does this catch too much? might want to restrict this a bit *)
      try [ translate_expr env e ] with InvalidIndexError _ -> [])
  | (name, t) :: vs ->
      possible_values_of_type env t
      |> List.map (fun v ->
             branch_on_free_vars
               { env with venv = (name, v) :: env.venv }
               vs e)
      |> List.fold_left ( @ ) []
  (* | _ -> raise (Failure "translate_expr: not yet implemented") *)

(* should only take in spec rule statements, outputs a list of smt-lib constraints *)
let rec translate_spec_stmt (ctx : type_context) (env : dynamic_environment)
    (spec : specification_statement) : string list =
  let branch e =
    let _, inferred = type_check ctx [] e (Some BooleanType) in
    let vs = branch_on_free_vars env inferred e in
    if debug then vs |> List.map show_vc_term |> List.iter print_endline;
    vs
  in
  match spec with
  | RequireStmt e -> List.map (fun v -> 
      s_expr_of [ "assert"; smt_of_predicate v ]) (branch e)
  | DisallowStmt e -> translate_spec_stmt ctx env (RequireStmt (Not e))
  | PreferStmt (e, w) -> 
    let vs = branch e in 
    List.map (fun v -> 
      s_expr_of [ "assert-soft"; smt_of_predicate v; ":weight"; string_of_float (float_of_int w /. float_of_int (List.length vs))]) vs
  | _ -> raise (Failure "interpret_spec_stmt: not yet implemented")

let translate_def_stmt (ctx : type_context) (env : dynamic_environment)
    (def : definition_statement) : type_context * dynamic_environment =
  match def with
  | ConstDefStmt (name, e) ->
      let t, _ = type_check ctx [] e None in
      let v = translate_expr env e in
      ( { ctx with vctx = (name, t) :: ctx.vctx },
        { env with venv = (name, v) :: env.venv } )
  | FuncDefStmt (name, params, body) -> (
      (* first add annotated types to context *)
      let annotated =
        params
        |> List.filter_map (fun (param, tref) ->
          match !tref with
          | Some t -> Some (param, t)
          | None -> None)
      in
      let scoped_ctx = { ctx with vctx = annotated @ ctx.vctx } in
      let t, inferred = type_check scoped_ctx [] body None in
      (* should be no unbound variables; all inferred types should be a parameter *)
      match
        List.find_opt
          (fun (name, _) -> not (List.mem_assoc name params))
          inferred
      with
      | Some (varname, _) -> raise (TypeError ("Unbound variable " ^ varname))
      | None ->
          let param_names = fst (List.split params) in
          let param_types =
            param_names
            |> List.map (fun param_name ->
                   match !(List.assoc param_name params) with
                   | Some param_type -> param_type
                   | None -> (
                       try List.assoc param_name inferred
                       with Not_found ->
                         raise
                           (TypeError
                              ("Unable to infer type of parameter " ^ param_name))
                       ))
          in
          ( { ctx with fctx = (name, (param_types, t)) :: ctx.fctx },
            { env with fenv = (name, (param_names, body)) :: env.fenv } ))

let translate_cfg_stmt (ctx : type_context) (env : dynamic_environment)
    (cfg : configuration_statement) : type_context * dynamic_environment =
  match cfg with
  | VoiceCfgStmt voices ->
      let n = List.length voices in
      if env.voices_declared then
        raise (RuntimeError "Voices have already been declared")
        (* need to make sure voice count matches existing voices, if midi file was included in input *)
      else if Option.is_some env.voice_count && Option.get env.voice_count <> n
      then raise (RuntimeError "Voice count mismatch")
      else
        ( {
            ctx with
            vctx = List.map (fun name -> (name, VoiceType)) voices @ ctx.vctx;
          },
          {
            env with
            voice_count = Some n;
            venv = List.mapi (fun i name -> (name, Voice i)) voices @ env.venv;
          } )
  | SongLengthUnitsCfgStmt l ->
      if
        Option.is_some env.song_length_units
        && Option.get env.song_length_units <> l
      then raise (RuntimeError "Song length units mismatch")
        (* TODO: add inference/checking logic for song_length_ticks and time_unit_ticks *)
        (* if time unit ticks have also been specified, make sure that this matches with the midi
           file's total ticks, or set it *)
        (* idk what's going on here

           else


               if Option.is_some env.time_unit_ticks
               then let time_unit_ticks = Option.get env.time_unit_ticks in
                 if Option.is_some env.song_length_ticks &&
                       l * Option.get env.time_unit_ticks <> Option.get env.song_length_ticks
                 then raise (RuntimeError "Song length ticks mismatch")
               else ctx, {env with song_length_units = l; song_length_ticks = } *)
      else 
        ({ ctx with
          vctx = ("end", TimeStepType) :: ctx.vctx
        }, 
        { env with 
          song_length_units = Some l;
          venv = ("end", TimeStep (l - 1)) :: env.venv })
  | TimeUnitTicksCfgStmt _ | KeyCfgStmt _ ->
      ignore ctx;
      raise (Failure "interpret_cfg_stmt: not yet implemented")

(* outputs 
more program (in the case included from another file),
updated context, environment, and smt program as a list of constraints *)
let translate_stmt (ctx : type_context) (env : dynamic_environment)
    (smt : string list) (stmt : statement) :
    program * type_context * dynamic_environment * string list =
  if debug then print_endline (show_statement stmt);
  match stmt with
  | ConfigurationStmt cfg ->
      let ctx, env = translate_cfg_stmt ctx env cfg in
      ([], ctx, env, smt)
  | DefinitionStmt def ->
      let ctx, env = translate_def_stmt ctx env def in
      ([], ctx, env, smt)
  | SpecificationStmt spec ->
      ([], ctx, env, smt @ ("; Specification" :: translate_spec_stmt ctx env spec))
  | IncludeStmt filename ->
      let channel = open_in filename in
      let lexbuf = Lexing.from_channel channel in
      let prog = Parser.prog Lexer.tokenize lexbuf in
      (prog, ctx, env, smt)

(* returns an smt-lib program and the final dynamic environment 
if synth is true, adds necessary constraints to synthesize
*)
let translate (env : dynamic_environment) 
              (prog : statement list) 
              (synth : bool)
              : string list * dynamic_environment =
  let rec aux (ctx : type_context) (env : dynamic_environment)
      (smt : string list) (prog : statement list) =
    match prog with
    | [] ->
        if debug then (
          (* TODO: remove? *)
          print_endline (show_type_context ctx);
          print_endline (show_dynamic_environment env));
        (* check all necessary configurations were present *)
        if Option.is_none env.voice_count then
          raise (RuntimeError "Voice count was not configured");
        if Option.is_none env.song_length_units then
          raise (RuntimeError "Song length units was not configured");
        let pitch_bounds = 
            if synth then assert_pitch_bounds (Option.get env.voice_count) (Option.get env.song_length_units)
            else []
        in
        (initialize_smt env @ smt @ pitch_bounds, env)
    | stmt :: prog ->
        let more_prog, new_ctx, new_env, new_smt = translate_stmt ctx env smt stmt in
        aux new_ctx new_env new_smt (more_prog @ prog)
  in
  let empty_ctx = { vctx = [("start", TimeStepType)]; fctx = [] } in
  aux empty_ctx env [] prog
