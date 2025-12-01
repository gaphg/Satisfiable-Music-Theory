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
let rec type_check (ctx : type_context)
                   (inferred : var_type_context)
                   (e : expr) 
                   (expected : sz_type option) 
                   : sz_type * var_type_context =
  let accumulate_check inferred e expected = 
    let _, new_inferred = type_check ctx inferred e expected in new_inferred
  in
  let (checked_type, new_inferred) =
    match e with
    | PitchLit _ -> PitchType, inferred
    | IntervalLit _ -> IntervalType, inferred
    | TimeStepLit _ -> TimeStepType, inferred
    | BooleanLit _ -> BooleanType, inferred
    | IntegerLit _ -> IntegerType, inferred
    | Var name -> begin
      match List.assoc_opt name ctx.vctx, List.assoc_opt name inferred, expected with
      | None, None, None -> raise (TypeError ("Unable to infer type of variable " ^ name))
      | None, None, Some t -> t, (name, t) :: inferred
      | Some t, _, _ 
      | _, Some t, _ -> t, inferred
    end
    | FuncCall (name, args) -> begin
      match List.assoc_opt name ctx.fctx with
      | Some (arg_ts, ret_t) -> 
        (* check children *)
        let new_inferred = 
        List.fold_left2 (fun inferred e expected -> accumulate_check inferred e expected) inferred args (List.map (fun t -> Some t) arg_ts) (* TODO: deal with non-matching lengths*)
        in
        ret_t, new_inferred
      | None -> raise (TypeError ("Undefined function " ^ name))
    end
    | ListExpr l -> begin
      let rec check_each_element inf elts elt_type =
        match elts with
        | [] -> ListType elt_type, inf
        | e :: elts -> check_each_element (accumulate_check inf e (Some elt_type)) elts elt_type
      in
      match l, expected with
      | [], None -> raise (TypeError "Unable to infer element type of empty list expression")
      | [], Some t -> t, inferred
      | e :: l, expected -> 
        let expected_elt_type = Option.map (function 
                                          | ListType t | TimeSeriesType t -> t 
                                          | t -> raise (TypeError ("Expected " ^ (string_of_expr e) ^ " to be of type " ^ (string_of_type t))))
        expected in
        (* let expected_elt_type = Option.map () *)
        (* TODO: this will not infer unbound vars in the element *)
        let elt_type, new_inferred = type_check ctx inferred e expected_elt_type in
        check_each_element new_inferred l elt_type
    end
    | Pitches e -> TimeSeriesType PitchType, (accumulate_check inferred e (Some VoiceType))
    | Contour e -> TimeSeriesType IntervalType, (accumulate_check inferred e (Some VoiceType))
    | Diads (e1, e2) -> TimeSeriesType IntervalType, (accumulate_check (accumulate_check inferred e1 (Some VoiceType)) e2 (Some VoiceType))

    | IntervalBetween (e1, e2) -> IntervalType, (accumulate_check (accumulate_check inferred e1 (Some PitchType)) e2 (Some PitchType))

    (* boolean ops *)
    | And (e1, e2) 
    | Or (e1, e2)
    | Implies (e1, e2) -> BooleanType, (accumulate_check (accumulate_check inferred e1 (Some BooleanType)) e2 (Some BooleanType))

    (* operations where lhs and rhs must be the same type but o/w unknown *)
    | Plus (e1, e2)
    | Minus (e1, e2) -> begin
      try let (lhs_t, new_inferred) = type_check ctx inferred e1 None
        in lhs_t, (accumulate_check new_inferred e2 (Some lhs_t))
      with TypeError _ -> let (rhs_t, new_inferred) = type_check ctx inferred e2 None
        in rhs_t, (accumulate_check new_inferred e1 (Some rhs_t))
    end
    | Equals (e1, e2) 
    | NotEquals (e1, e2) -> begin
      try let (lhs_t, new_inferred) = type_check ctx inferred e1 None
        in BooleanType, (accumulate_check new_inferred e2 (Some lhs_t))
      with TypeError _ -> let (rhs_t, new_inferred) = type_check ctx inferred e2 None
        in BooleanType, (accumulate_check new_inferred e1 (Some rhs_t))
    end
    (* TODO: refactor code above (checking left and right) *)

    | ElementAt (list, idx) -> begin
      try let lhs_type, new_inferred = type_check ctx inferred list None in
        match lhs_type with
        | TimeSeriesType t -> t, (accumulate_check new_inferred idx (Some TimeStepType))
        | ListType t -> t, (accumulate_check new_inferred idx (Some IntegerType))
        | _ -> raise (TypeError ("Expected " ^ (string_of_expr list) ^ " to be of type TimeSeries or List"))
      with TypeError _ -> 
        let rhs_type, new_inferred = type_check ctx inferred idx None in
        let collection = match rhs_type with
        | TimeStepType -> fun t -> TimeSeriesType t
        | IntegerType -> fun t -> ListType t
        | _ -> raise (TypeError "asdf")
        in
        let expected_list_type = Option.map collection expected in
        let lhs_type, final_inferred = type_check ctx new_inferred list expected_list_type in
        match lhs_type with
        | TimeSeriesType t | ListType t -> t, final_inferred
        | _ -> raise (TypeError ("Expected " ^ (string_of_expr list) ^ " to be of type TimeSeries or List"))
    end
    | Contains (e1, e2) -> begin
      try let lhs_type, new_inferred = type_check ctx inferred e1 None in
        match lhs_type with
        | TimeSeriesType t | ListType t ->
          BooleanType, (accumulate_check new_inferred e2 (Some t))
        | _ -> raise (TypeError ("Expected " ^ (string_of_expr e1) ^ " to be of type TimeSeries or List"))
      with TypeError _ -> let rhs_type, new_inferred = type_check ctx inferred e2 None in
        BooleanType, (accumulate_check new_inferred e1 (Some (ListType rhs_type)))
    end

    | _ -> raise (Failure ("type_check: not yet implemented " ^ (show_expr e)))
  in 
  match expected with
  | Some t ->
    if t = checked_type then (checked_type, new_inferred)
    else raise (TypeError ("Expected " ^ 
                (string_of_expr e) ^ " to be of type " ^ 
                (string_of_type t) ^ " but found " ^ (string_of_type checked_type)))
                  
  | None -> checked_type, new_inferred