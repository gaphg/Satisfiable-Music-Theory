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
  | TimeSeriesType t -> string_of_type t ^ " TimeSeries"
  | ListType t -> string_of_type t ^ " List"

type func_type = sz_type list * sz_type [@@deriving show]
type var_type_context = (string * sz_type) list [@@deriving show]
type func_type_context = (string * func_type) list [@@deriving show]

type type_context = { vctx : var_type_context; fctx : func_type_context }
[@@deriving show]
