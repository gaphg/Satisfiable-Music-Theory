open Bachend
open Ast
open Errors
open Types
open Smt_lib

let rec interpret_expr (env : dynamic_environment) (e : expr) : value =
  (* print_endline (show_dynamic_environment env); *)
  match e with
  (* literals *)
  | PitchLit p -> Pitch p
  | IntervalLit i -> Interval i
  | TimeStepLit t -> TimeStep t
  | BooleanLit b -> Boolean b

  (* variables/functions *)
  | Var name -> begin
    match lookup env.venv name with
    | Some v -> v
    | None -> raise (RuntimeError ("unbound variable" ^ name))
  end

  (* builtin functions on voices *)
  | Pitches v -> begin
    match interpret_expr env v, env.song_length_units with
    | Voice v, Some song_length_units -> 
      TimeSeries (List.init song_length_units (fun t -> SymbolicPitch (v, t)))
    | Voice _, None -> raise (RuntimeError "Song length units has not been configured/cannot be determined")
    | _ -> raise (Failure "interpret_expr: impossible")
  end

  | PlusExpr (e1, e2) -> begin
    match interpret_expr env e1, interpret_expr env e2 with
    | TimeStep t1, TimeStep t2 -> TimeStep (t1 + t2)
    | _ -> raise (Failure "interpret_expr: not yet implemented")
  end

  (* comparisons *)
  | EqualsExpr (e1, e2) -> Equals (interpret_expr env e1, interpret_expr env e2)

  | ElementAt (list, idx) -> begin
    match interpret_expr env list, interpret_expr env idx with
    | TimeSeries l, TimeStep i | SzList l, Integer i ->
      if i < 0 || List.length l <= i
        then raise (InvalidIndexError i)
      else List.nth l i
    | _ -> raise (Failure "interpret_expr: impossible")

  end
  | _ -> raise (Failure "interpret_expr: not yet implemented")

let all_pitches = List.init 128 (fun n -> Pitch n)
let possible_values_of_type (env : dynamic_environment)
                            (t : sz_type)
                            : value list =
  match t with
  | VoiceType -> begin
    match env.voice_count with
    | Some n -> List.init n (fun i -> Voice i)
    | None -> raise (RuntimeError "Voice count has not been configured/no midi file input")
  end
  | PitchType -> all_pitches
  | TimeStepType -> begin
    match env.song_length_units with
    | Some n -> List.init n (fun i -> TimeStep i)
    | None -> raise (RuntimeError "Song length units has not been configured/could not be determined")
  end
  | BooleanType -> [Boolean true; Boolean false]
  | _ -> raise (Failure "possible_values_of_type: not yet implemented")

(* for each free variable, interprets the expression for all possible values of that variable, outputting a list of values TODO: decide whether to use forall smt quantifier (not sure if it's supported as well, gotta research )*)
let rec branch_on_free_vars (env : dynamic_environment)
                            (inferred : var_type_context)
                            (e : expr)
                            : value list = 
  match inferred with
  | [] -> begin
    (* does this catch too much? might want to restrict this a bit *)
    try [interpret_expr env e] with InvalidIndexError _ -> []
  end
  | (name, t) :: inferred -> 
    possible_values_of_type env t
    |> List.map (fun v -> branch_on_free_vars {env with venv = (name, v) :: env.venv} inferred e)
    |> List.fold_left (@) []

(* should only take in spec rule statements, outputs a list of smt-lib constraints *)
let interpret_spec_stmt (ctx : type_context)
                        (env : dynamic_environment) 
                        (spec : specification_statement)
                        : string list =                   
  match spec with
  | RequireStmt e -> begin
    let (_, inferred) = type_check ctx [] e (Some BooleanType) in
    let vs = branch_on_free_vars env inferred e in
    List.map (fun v -> s_expr_of ["assert"; smt_of_predicate v]) vs
  end
  | _ -> raise (Failure "interpret_spec_stmt: not yet implemented")

let interpret_def_stmt (ctx : type_context)
                        (env : dynamic_environment)
                        (def : definition_statement)
                        : type_context * dynamic_environment =
  match def with
  | ConstDefStmt (name, e) -> begin
    let t, _ = type_check ctx [] e None in
    let v = interpret_expr env e in
    {ctx with vctx = (name, t) :: ctx.vctx}, {env with venv = (name, v) :: env.venv}
  end
  | _ -> raise (Failure "interpret_defn_stmt: not yet implemented")

let interpret_cfg_stmt (ctx : type_context)
                       (env : dynamic_environment)
                       (cfg : configuration_statement)
                       : type_context * dynamic_environment =
  match cfg with
  | VoiceCfgStmt voices -> begin
    let n = List.length voices in
    if env.voices_declared 
      then raise (RuntimeError "Voices have already been declared")
    (* need to make sure voice count matches existing voices, if midi file was included in input *)
    else if Option.is_some env.voice_count && Option.get env.voice_count != n
      then raise (RuntimeError "Voice count mismatch")
    else
      {ctx with 
        vctx = (List.map (fun name -> (name, VoiceType)) voices) @ ctx.vctx
      },
      {env with 
        voice_count = Some n; 
        venv = (List.mapi (fun i name -> (name, Voice i)) voices) @ env.venv
      }
  end
  | SongLengthUnitsCfgStmt l -> begin
    if Option.is_some env.song_length_units && Option.get env.song_length_units != l
      then raise (RuntimeError "Song length units mismatch")
    (* TODO: add inference/checking logic for song_length_ticks and time_unit_ticks *)
    (* if time unit ticks have also been specified, make sure that this matches with the midi
       file's total ticks, or set it *)
    (*  idk what's going on here
  
  else
      
      
      if Option.is_some env.time_unit_ticks
      then let time_unit_ticks = Option.get env.time_unit_ticks in
        if Option.is_some env.song_length_ticks && 
              l * Option.get env.time_unit_ticks != Option.get env.song_length_ticks
        then raise (RuntimeError "Song length ticks mismatch")
      else ctx, {env with song_length_units = l; song_length_ticks = } *)
    else
      ctx,
      {env with
        song_length_units = Some l
      }
  end
  | TimeUnitTicksCfgStmt _
  | KeyCfgStmt _ -> ignore ctx; raise (Failure "interpret_cfg_stmt: not yet implemented")

(* outputs updated context, environment, and smt program as a list of constraints *)
let interpret_stmt (ctx : type_context)
                   (env : dynamic_environment)
                   (smt : string list)
                   (stmt : statement)
                   : type_context * dynamic_environment * string list =
  match stmt with
  | ConfigurationStmt cfg -> 
    let (ctx, env) = interpret_cfg_stmt ctx env cfg in
    ctx, env, smt
  | DefinitionStmt def -> 
    let (ctx, env) = interpret_def_stmt ctx env def in
    ctx, env, smt
  | SpecificationStmt spec -> ctx, env, smt @ (interpret_spec_stmt ctx env spec)

let const_name_of_voice_time v t =
 "v" ^ (string_of_int v) ^ "t" ^ (string_of_int t)

let declare_symbols (env : dynamic_environment) : string list =
  match env.voice_count, env.song_length_units with
  | Some voice_count, Some song_length_units -> 
    List.init 
      (voice_count * song_length_units) 
      (fun n ->
        let v = n / song_length_units in
        let t = n mod song_length_units in
        let const_name = const_name_of_voice_time v t in
        s_expr_of ["declare-const"; const_name; "(_ BitVec 7)"])
  | _ -> raise (Failure "declare_symbols: not yet implemented")


let interpret (env : dynamic_environment)
              (prog : statement list)
              : string list =
  let rec aux (ctx : type_context)
              (env : dynamic_environment)
              (smt : string list)
              (prog : statement list) =
    match prog with
    | [] -> 
      if debug then (* TODO: remove? *)
        print_endline (show_type_context ctx);
        print_endline (show_dynamic_environment env);
      declare_symbols env @ smt
    | stmt :: prog -> 
      let new_ctx, new_env, new_smt = interpret_stmt ctx env smt stmt in
      aux new_ctx new_env new_smt prog 
  in
  let empty_ctx = { vctx = []; fctx = [] } in
  aux empty_ctx env [] prog