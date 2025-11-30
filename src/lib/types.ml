open Ast
open Errors

type sz_type = 
| VoiceType
| PitchType
| IntervalType
(* | Chord *)
| TimeStepType
| IntegerType (* mostly used for indexing into list *)
| BooleanType
| TimeSeriesType of sz_type
| ListType of sz_type
[@@deriving show]
(*| Unknown (* only for use in intermediate states of typechecking *)*)

let rec string_of_type (t : sz_type) =
  match t with
  | VoiceType -> "Voice"
  | PitchType -> "Pitch"
  | IntervalType -> "Interval"
  (* | Chord -> "Chord" *)
  | TimeStepType -> "TimeStep"
  | IntegerType -> "Integer"
  | BooleanType -> "Boolean"
  | TimeSeriesType t -> (string_of_type t) ^ "TimeSeries"
  | ListType t -> (string_of_type t) ^ " List"

type func_type = sz_type list * sz_type
[@@deriving show]

type var_type_context = (string * sz_type) list
[@@deriving show]

type func_type_context = (string * func_type) list
[@@deriving show]

type type_context = {
  vctx : var_type_context;
  fctx : func_type_context;
}
[@@deriving show]


let rec lookup l name =
  match l with
  | [] -> None
  | (x, value) :: l ->
    if x = name then Some value else lookup l name

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
    | Var name -> begin
      match lookup ctx.vctx name, lookup inferred name, expected with
      | None, None, None -> raise (TypeError ("Unable to infer type of variable " ^ name))
      | None, None, Some t -> t, (name, t) :: inferred
      | Some t, _, _ 
      | _, Some t, _ -> t, inferred
    end
    | FuncCall (name, args) -> begin
      match lookup ctx.fctx name with
      | Some (arg_ts, ret_t) -> 
        (* check children *)
        let new_inferred = 
        List.fold_left2 (fun inferred e expected -> accumulate_check inferred e expected) inferred args (List.map (fun t -> Some t) arg_ts) (* TODO: deal with non-matching lengths*)
        in
        ret_t, new_inferred
      | None -> raise (TypeError ("Undefined function " ^ name))
    end

    | Pitches e -> TimeSeriesType PitchType, (accumulate_check inferred e (Some VoiceType))
    | Contour e -> TimeSeriesType IntervalType, (accumulate_check inferred e (Some VoiceType))
    | Diads (e1, e2) -> TimeSeriesType IntervalType, (accumulate_check (accumulate_check inferred e1 (Some VoiceType)) e2 (Some VoiceType))

    (* boolean ops *)

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
      let lhs_type, new_inferred = type_check ctx inferred list None in
      match lhs_type with
      | TimeSeriesType t -> t, (accumulate_check new_inferred idx (Some TimeStepType))
      | ListType t -> t, (accumulate_check new_inferred idx (Some IntegerType))
      | _ -> raise (TypeError ("Expected " ^ (string_of_expr list) ^ " to be of type TimeSeries or List"))
    end

    | _ -> raise (Failure "type_check: not yet implemented")
  in 
  match expected with
  | Some t ->
    if t = checked_type then (checked_type, new_inferred)
    else raise (TypeError ("Expected " ^ 
                (string_of_expr e) ^ " to be of type " ^ 
                (string_of_type t) ^ " but found " ^ (string_of_type checked_type)))
                  
  | None -> checked_type, new_inferred