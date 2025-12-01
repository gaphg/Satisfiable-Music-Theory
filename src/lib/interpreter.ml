open Bachend
open Ast
open Types
open Errors
open Type_checker
open Smt_lib

let rec interpret_expr (env : dynamic_environment) (e : expr) : value =
  match e with
  | SymbolicPitchExpr (v, t) -> SymbolicPitch (v, t)
  | SymbolicIntervalExpr ((v1, t1), (v2, t2)) ->
      SymbolicInterval (SymbolicPitch (v1, t1), SymbolicPitch (v2, t2))
  (* literals *)
  | PitchLit p -> Pitch p
  | IntervalLit (i, d) -> Interval (i, d)
  | TimeStepLit t -> TimeStep t
  | BooleanLit b -> Boolean b
  | IntegerLit i -> Integer i
  (* variables/functions *)
  | Var name -> (
      match List.assoc_opt name env.venv with
      | Some v -> v
      | None -> raise (RuntimeError ("unbound variable " ^ name)))
  | FuncCall (fname, args) ->
      let arg_names, body = List.assoc fname env.fenv in
      let scoped_venv =
        List.combine arg_names (List.map (interpret_expr env) args)
      in
      let scoped_env = { env with venv = scoped_venv @ env.venv } in
      interpret_expr scoped_env body
  | ListExpr l -> SzList l
  (* builtin functions on voices *)
  | Pitches v -> (
      match (interpret_expr env v, env.song_length_units) with
      | Voice v, Some song_length_units ->
          TimeSeries
            (List.init song_length_units (fun t -> SymbolicPitchExpr (v, t)))
      | Voice _, None ->
          raise
            (RuntimeError
               "Song length units has not been configured/cannot be determined")
      | _ -> raise (Failure "interpret_expr: impossible"))
  | Contour v -> (
      match (interpret_expr env v, env.song_length_units) with
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
        (interpret_expr env v1, interpret_expr env v2, env.song_length_units)
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
      SymbolicInterval (interpret_expr env p1, interpret_expr env p2)
  | Plus (e1, e2) -> (
      match (interpret_expr env e1, interpret_expr env e2) with
      | TimeStep t1, TimeStep t2 -> TimeStep (t1 + t2)
      | _ -> raise (Failure "interpret_expr: not yet implemented"))
  | And (e1, e2) -> SymbolicAnd [ interpret_expr env e1; interpret_expr env e2 ]
  | Or (e1, e2) -> SymbolicOr [ interpret_expr env e1; interpret_expr env e2 ]
  | Implies (e1, e2) ->
      SymbolicImplies (interpret_expr env e1, interpret_expr env e2)
  (* comparisons *)
  | Equals (e1, e2) -> (
      let v1, v2 = (interpret_expr env e1, interpret_expr env e2) in
      (* deal with some wiggle room for equality *)
      match (v1, v2) with
      | Interval (_, None), _ | _, Interval (_, None) ->
          SymbolicEquals (SymbolicAbs v1, SymbolicAbs v2)
      | _ -> SymbolicEquals (v1, v2))
  | ElementAt (list, idx) -> (
      match (interpret_expr env list, interpret_expr env idx) with
      | TimeSeries l, TimeStep i | SzList l, Integer i ->
          if i < 0 || List.length l <= i then raise (InvalidIndexError i)
          else interpret_expr env (List.nth l i)
      | _ -> raise (Failure "interpret_expr: impossible"))
  | Contains (list, elt) -> (
      match interpret_expr env list with
      | SzList es | TimeSeries es ->
          SymbolicOr
            (List.map (fun e -> interpret_expr env (Equals (e, elt))) es)
      | _ -> raise (Failure "interpret_expr: impossible"))
  | _ -> raise (Failure ("interpret_expr: not yet implemented " ^ show_expr e))

let all_pitches = List.init 128 (fun n -> Pitch n)

let possible_values_of_type (env : dynamic_environment) (t : sz_type) :
    value list =
  match t with
  | VoiceType -> (
      match env.voice_count with
      | Some n -> List.init n (fun i -> Voice i)
      | None ->
          raise
            (RuntimeError
               "Voice count has not been configured/no midi file input"))
  | PitchType -> all_pitches
  | TimeStepType -> (
      match env.song_length_units with
      | Some n -> List.init n (fun i -> TimeStep i)
      | None ->
          raise
            (RuntimeError
               "Song length units has not been configured/could not be \
                determined"))
  | BooleanType -> [ Boolean true; Boolean false ]
  | _ -> raise (Failure "possible_values_of_type: not yet implemented")

(* for each free variable, interprets the expression for all possible values of that variable, outputting a list of values TODO: decide whether to use forall smt quantifier (not sure if it's supported as well, gotta research )*)
let rec branch_on_free_vars (env : dynamic_environment)
    (inferred : var_type_context) (e : expr) : value list =
  match inferred with
  | [] -> (
      (* does this catch too much? might want to restrict this a bit *)
      try [ interpret_expr env e ] with InvalidIndexError _ -> [])
  | (name, t) :: inferred ->
      possible_values_of_type env t
      |> List.map (fun v ->
             branch_on_free_vars
               { env with venv = (name, v) :: env.venv }
               inferred e)
      |> List.fold_left ( @ ) []

(* should only take in spec rule statements, outputs a list of smt-lib constraints *)
let interpret_spec_stmt (ctx : type_context) (env : dynamic_environment)
    (spec : specification_statement) : string list =
  match spec with
  | RequireStmt e ->
      let _, inferred = type_check ctx [] e (Some BooleanType) in
      let vs = branch_on_free_vars env inferred e in
      if debug then vs |> List.map show_value |> List.iter print_endline;
      List.map (fun v -> s_expr_of [ "assert"; smt_of_predicate v ]) vs
  | _ -> raise (Failure "interpret_spec_stmt: not yet implemented")

let interpret_def_stmt (ctx : type_context) (env : dynamic_environment)
    (def : definition_statement) : type_context * dynamic_environment =
  match def with
  | ConstDefStmt (name, e) ->
      let t, _ = type_check ctx [] e None in
      let v = interpret_expr env e in
      ( { ctx with vctx = (name, t) :: ctx.vctx },
        { env with venv = (name, v) :: env.venv } )
  | FuncDefStmt (name, params, body) -> (
      (* first add annotated types to context *)
      let annotated =
        params
        |> List.filter_map (fun (param, t) ->
               Option.map (fun tconcrete -> (param, tconcrete)) t)
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
                   match List.assoc param_name params with
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

let interpret_cfg_stmt (ctx : type_context) (env : dynamic_environment)
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
      else (ctx, { env with song_length_units = Some l })
  | TimeUnitTicksCfgStmt _ | KeyCfgStmt _ ->
      ignore ctx;
      raise (Failure "interpret_cfg_stmt: not yet implemented")

(* outputs updated context, environment, and smt program as a list of constraints *)
let interpret_stmt (ctx : type_context) (env : dynamic_environment)
    (smt : string list) (stmt : statement) :
    type_context * dynamic_environment * string list =
  match stmt with
  | ConfigurationStmt cfg ->
      let ctx, env = interpret_cfg_stmt ctx env cfg in
      (ctx, env, smt)
  | DefinitionStmt def ->
      let ctx, env = interpret_def_stmt ctx env def in
      (ctx, env, smt)
  | SpecificationStmt spec ->
      (ctx, env, smt @ ("; Specification" :: interpret_spec_stmt ctx env spec))

let interpret (env : dynamic_environment) (prog : statement list) : string list
    =
  let rec aux (ctx : type_context) (env : dynamic_environment)
      (smt : string list) (prog : statement list) =
    match prog with
    | [] ->
        if debug then (* TODO: remove? *)
          print_endline (show_type_context ctx);
        print_endline (show_dynamic_environment env);
        initialize_smt env @ smt @ [ "(check-sat)"; "(get-model)" ]
    | stmt :: prog ->
        let new_ctx, new_env, new_smt = interpret_stmt ctx env smt stmt in
        aux new_ctx new_env new_smt prog
  in
  let empty_ctx = { vctx = []; fctx = [] } in
  aux empty_ctx env [] prog
