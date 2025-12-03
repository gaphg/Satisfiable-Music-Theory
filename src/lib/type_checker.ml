open Ast
open Types
open Errors

(* let rec lookup l name =
   match l with
   | [] -> None
   | (x, value) :: l ->
     if x = name then Some value else lookup l name *)

(* TODO: guarantee that returned inferred list has all distinct variable names (pretty sure it's true) *)
(* type-checks expression, inferring the types of free variables *)
let rec type_check (ctx : type_context) (inferred : var_type_context) (e : expr)
    (expected : sz_type option) : sz_type * var_type_context =
  (* helpers *)
  (* accumulates the inferred variables *)
  let accumulate_check inferred e expected =
    let _, new_inferred = type_check ctx inferred e expected in
    new_inferred
  in
  (* checks left and right side of a polymorphic expression that requires two sides to be of same type. 
    returns type and inferred variables
  *)
  let check_lr_same lhs rhs =
    try
          let lhs_t, new_inferred = type_check ctx inferred lhs None in
          (BooleanType, accumulate_check new_inferred rhs (Some lhs_t))
        with TypeInferenceError _ ->
          let rhs_t, new_inferred = type_check ctx inferred rhs None in
          (BooleanType, accumulate_check new_inferred lhs (Some rhs_t))
  in
  let checked_type, new_inferred =
    match e with
    | PitchLit _ -> (PitchType, inferred)
    | IntervalLit _ -> (IntervalType, inferred)
    | TimeStepLit _ -> (TimeStepType, inferred)
    | IntegerLit _ -> (IntegerType, inferred)
    | BooleanLit _ -> (BooleanType, inferred)
    | DirectionLit _ -> (DirectionType, inferred)
    | Var name -> (
        match
          (List.assoc_opt name ctx.vctx, List.assoc_opt name inferred, expected)
        with
        | None, None, None ->
            raise (TypeInferenceError ("Unable to infer type of variable " ^ name))
        | None, None, Some t -> (t, (name, t) :: inferred)
        | Some t, _, _ | _, Some t, _ -> (t, inferred))
    | FuncCall (name, args) -> (
        match List.assoc_opt name ctx.fctx with
        | Some (arg_ts, ret_t) ->
            (* check children *)
            let new_inferred =
              List.fold_left2
                (fun inferred e expected ->
                  accumulate_check inferred e expected)
                inferred args
                (List.map (fun t -> Some t) arg_ts)
              (* TODO: deal with non-matching lengths*)
            in
            (ret_t, new_inferred)
        | None -> raise (TypeError ("Undefined function " ^ name)))
    | ListExpr l -> (
        let rec check_each_element inf elts elt_type =
          match elts with
          | [] -> (ListType elt_type, inf)
          | e :: elts ->
              check_each_element
                (accumulate_check inf e (Some elt_type))
                elts elt_type
        in
        match (l, expected) with
        | [], None ->
            raise
              (TypeError "Unable to infer element type of empty list expression")
        | [], Some t -> (t, inferred)
        | e :: l, expected ->
            let expected_elt_type =
              Option.map
                (function
                  | ListType t | TimeSeriesType t -> t
                  | t ->
                      raise
                        (TypeError
                           ("Expected " ^ string_of_expr e ^ " to be of type "
                          ^ string_of_type t)))
                expected
            in
            (* let expected_elt_type = Option.map () *)
            (* TODO: this will not infer unbound vars in the element *)
            let elt_type, new_inferred =
              type_check ctx inferred e expected_elt_type
            in
            check_each_element new_inferred l elt_type)
    | Pitches e ->
        (TimeSeriesType PitchType, accumulate_check inferred e (Some VoiceType))
    | Contour e ->
        ( TimeSeriesType IntervalType,
          accumulate_check inferred e (Some VoiceType) )
    | Diads (e1, e2) ->
        ( TimeSeriesType IntervalType,
          accumulate_check
            (accumulate_check inferred e1 (Some VoiceType))
            e2 (Some VoiceType) )
    | IntervalBetween (e1, e2) ->
        ( IntervalType,
          accumulate_check
            (accumulate_check inferred e1 (Some PitchType))
            e2 (Some PitchType) )
    (* boolean ops *)
    | Not e -> (BooleanType, accumulate_check inferred e (Some BooleanType))
    | And (e1, e2) 
    | Or (e1, e2) 
    | Implies (e1, e2) 
    | Iff (e1, e2) ->
        ( BooleanType,
          accumulate_check
            (accumulate_check inferred e1 (Some BooleanType))
            e2 (Some BooleanType) )
    | Exists (vars, e)
    | Forall (vars, e) -> (
      (* add labeled var types to context *)
      let annotated =
        vars
        |> List.filter_map (fun (v, t) ->
               Option.map (fun tconcrete -> (v, tconcrete)) t)
      in
      let scoped_ctx = { ctx with vctx = annotated @ ctx.vctx } in 
      let _, inf = type_check scoped_ctx inferred e (Some BooleanType) in
      (* if types of vars bound by this expr were inferred, remove from final inferred list *)
      let pruned_inf =
      vars
      |> List.filter (fun p -> Option.is_none (snd p))
      |> List.map fst
      |> List.fold_left (Fun.flip List.remove_assoc) inf in
      (BooleanType, pruned_inf)
    )
    (* operations where lhs and rhs must be the same type but o/w unknown *)
    | Plus (e1, e2) 
    | Minus (e1, e2) -> (
        try
          let lhs_t, new_inferred = type_check ctx inferred e1 None in
          (lhs_t, accumulate_check new_inferred e2 (Some lhs_t))
        with TypeInferenceError _ ->
          let rhs_t, new_inferred = type_check ctx inferred e2 None in
          (rhs_t, accumulate_check new_inferred e1 (Some rhs_t)))
    | Equals (e1, e2) 
    | NotEquals (e1, e2)
    | LessThan (e1, e2)
    | LessThanEq (e1, e2)
    | GreaterThan (e1, e2)
    | GreaterThanEq (e1, e2) -> check_lr_same e1 e2
    | EqualsModOctave (e1, e2)
    | NotEqualsModOctave (e1, e2) ->(
        let t, inferred = check_lr_same e1 e2 in
        (match t with
        | PitchType | IntervalType -> (t, inferred)
        | _ -> raise (TypeError ("Expected operands of " ^ (string_of_expr e) ^ " to be of type Pitch or Interval"))))

    | Flatten e -> (
      match expected with
        | Some (PitchType | IntervalType as t) -> (t, accumulate_check inferred e (Some t))
        | Some t -> raise (TypeError ("Flatten returns type Pitch or Interval, but expected " ^ (string_of_type t)))
        | None -> 
          let t, inf = type_check ctx inferred e None in
          match t with
          | PitchType | IntervalType -> (t, inf)
          | _ -> raise (TypeError ("Expected " ^ (string_of_expr e) ^ " to be of type Pitch or Interval, but got " ^ (string_of_type t))))
    | ElementAt (list, idx) -> (
        try
          let lhs_type, new_inferred = type_check ctx inferred list None in
          match lhs_type with
          | TimeSeriesType t ->
              (t, accumulate_check new_inferred idx (Some TimeStepType))
          | ListType t ->
              (t, accumulate_check new_inferred idx (Some IntegerType))
          | _ ->
              raise
                (TypeError
                   ("Expected " ^ string_of_expr list
                  ^ " to be of type TimeSeries or List"))
        with TypeInferenceError _ -> (
          let rhs_type, new_inferred = type_check ctx inferred idx None in
          let collection =
            match rhs_type with
            | TimeStepType -> fun t -> TimeSeriesType t
            | IntegerType -> fun t -> ListType t
            | _ -> raise (TypeError ("Expected " ^ (string_of_expr idx) ^ " to be of type TimeStep or Integer, but got " ^ (string_of_type rhs_type)))
          in
          let expected_list_type = Option.map collection expected in
          let lhs_type, final_inferred =
            type_check ctx new_inferred list expected_list_type
          in
          match lhs_type with
          | TimeSeriesType t | ListType t -> (t, final_inferred)
          | _ ->
              raise
                (TypeError
                   ("Expected " ^ string_of_expr list
                  ^ " to be of type TimeSeries or List"))))
    | Contains (e1, e2) -> (
        try
          let lhs_type, new_inferred = type_check ctx inferred e1 None in
          match lhs_type with
          | TimeSeriesType t | ListType t ->
              (BooleanType, accumulate_check new_inferred e2 (Some t))
          | _ ->
              raise
                (TypeError
                   ("Expected " ^ string_of_expr e1
                  ^ " to be of type TimeSeries or List"))
        with TypeInferenceError _ ->
          let rhs_type, new_inferred = type_check ctx inferred e2 None in
          ( BooleanType,
            accumulate_check new_inferred e1 (Some (ListType rhs_type)) ))
    (* dummy nodes *)
    | SymbolicPitchExpr _ -> (PitchType, inferred)
    | SymbolicIntervalExpr _ -> (IntegerType, inferred)
  in
  match expected with
  | Some t ->
      if t = checked_type then (checked_type, new_inferred)
      else
        raise
          (TypeError
             ("Expected " ^ string_of_expr e ^ " to be of type "
            ^ string_of_type t ^ " but found "
             ^ string_of_type checked_type))
  | None -> (checked_type, new_inferred)
