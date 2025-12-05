type st_type =
  | VoiceType
  | PitchType
  | IntervalType
  | DirectionType
  (* | Chord *)
  | TimeStepType
  | IntegerType (* mostly used for indexing into list *)
  | BooleanType
  | TimeSeriesType of st_type
  | ListType of st_type
[@@deriving show]
(*| Unknown (* only for use in intermediate states of typechecking *)*)

let rec string_of_type (t : st_type) =
  match t with
  | VoiceType -> "Voice"
  | PitchType -> "Pitch"
  | IntervalType -> "Interval"
  | DirectionType -> "Direction"
  (* | Chord -> "Chord" *)
  | TimeStepType -> "TimeStep"
  | IntegerType -> "Integer"
  | BooleanType -> "Boolean"
  | TimeSeriesType t -> string_of_type t ^ " TimeSeries"
  | ListType t -> string_of_type t ^ " List"

type func_type = st_type list * st_type [@@deriving show]
type var_type_context = (string * st_type) list [@@deriving show]
type func_type_context = (string * func_type) list [@@deriving show]

type type_context = { vctx : var_type_context; fctx : func_type_context }
[@@deriving show]
