open Types

type expr =
  (* literals *)
  | PitchLit of int
  | IntervalLit of
      int * bool option (* user may specify direction, true -> ascending *)
  | TimeStepLit of int
  | IntegerLit of int
  | BooleanLit of bool
  | DirectionLit of bool
  (* only can specify concrete direction, not unspecified *)
  (* this means in syntax we need a way of differentiating literals for what type they are,
     e.g 1p 1i 1t
      C4 = 60p
  *)
  | Var of string
  | FuncCall of string * expr list (* function name, arguments *)
  | ListExpr of expr list
  (* builtin functions on voices *)
  | Pitches of expr (* voice -> pitch list *)
  | Contour of expr (* voice -> interval list *)
  | Diads of expr * expr (* v1, v2 -> interval list *)
  (*| Stack of expr (* timeStep -> pitch list *)*)
  | IntervalBetween of expr * expr (* p1, p2 -> interval *)
  (* int/timestep maybe can be extend to pitches/intervals too *)
  | Plus of expr * expr
  | Minus of expr * expr (* perhaps pitch - pitch gives interval? *)
  | Mod of expr * expr
  (* boolean/predicate operations *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Implies of expr * expr
  | Iff of expr * expr
  (* exists/forall: variables in lists, predicate *)
  | Exists of (string * expr) list * expr
  | Forall of (string * expr) list * expr
  (* comparison operations *)
  | Equals of expr * expr
  | NotEquals of expr * expr
  | LessThan of expr * expr
  | LessThanEq of expr * expr
  | GreaterThan of expr * expr
  | GreaterThanEq of expr * expr
  (* other builtins *)
  | ElementAt of expr * expr (* list, index -> element *)
  | Contains of expr * expr (* list, element -> bool *)
  | Flatten of expr
  | EqualsModOctave of expr * expr (* pitch, pitch -> bool *)
  | NotEqualsModOctave of expr * expr
  | Range of expr * expr (* T, T -> T list, where T is an integer-like type *)
  (* for internal implementation *)
  | SymbolicPitchExpr of int * int
  | SymbolicIntervalExpr of (int * int) * (int * int)
[@@deriving show]

type configuration_statement =
  | VoiceCfgStmt of string list
  | TimeUnitTicksCfgStmt of int
  | SongLengthUnitsCfgStmt of int
  | KeyCfgStmt of expr (* type: pitch *)
[@@deriving show]

type definition_statement =
  (* const name, const definition *)
  | ConstDefStmt of string * expr
  (* function name, function arguments/type annotations, function body *)
  | FuncDefStmt of string * (string * st_type option ref) list * expr
(* "expr" below should be type predicate *)
[@@deriving show]

type specification_statement =
  | RequireStmt of expr
  | DisallowStmt of expr
  | PreferStmt of expr * int (* constraint, weight *)
  | AvoidStmt of expr * int (* constraint, weight *)
[@@deriving show]

type statement =
  | ConfigurationStmt of configuration_statement * Lexing.position
  | DefinitionStmt of definition_statement * Lexing.position
  | SpecificationStmt of specification_statement * Lexing.position
  | IncludeStmt of string * Lexing.position(* filename of included script *)


type program = statement list

let pitch_string_of_int p =
  (List.nth ["C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B"] (p mod 12))
  ^ (string_of_int (p / 12 - 1))
let rec string_of_expr = function
| PitchLit p -> (string_of_int p) ^ "p"
| IntervalLit (i, None) -> (string_of_int i) ^ "i"
| IntervalLit (i, Some true) -> (string_of_int i) ^ "i up"
| IntervalLit (i, Some false) -> (string_of_int i) ^ "i down"
| TimeStepLit t -> (string_of_int t) ^ "t"
| IntegerLit n -> string_of_int n
| BooleanLit b -> string_of_bool b
| DirectionLit d -> if d then "up" else "down"
| Var name -> name
| FuncCall (name, args) ->
    name ^ "(" ^ (String.concat ", " (List.map string_of_expr args)) ^ ")"
| ListExpr lst ->
    "[" ^ (String.concat ", " (List.map string_of_expr lst)) ^ "]"
| Pitches e -> "pitches-of " ^ string_of_expr e
| Contour e -> "contour-of " ^ string_of_expr e
| Diads (e1, e2) -> "diads-of " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
| IntervalBetween (e1, e2) ->
    "interval-bt (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Plus (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
| Minus (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
| Mod (e1, e2) -> 
    "(" ^ string_of_expr e1 ^ " mod " ^ string_of_expr e2 ^ ")"
| Not e -> "not (" ^ string_of_expr e ^ ")"
| And (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " and " ^ string_of_expr e2 ^ ")"
| Or (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " or " ^ string_of_expr e2 ^ ")"
| Implies (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " => " ^ string_of_expr e2 ^ ")"
| Iff (e1, e2) -> 
    "(" ^ string_of_expr e1 ^ " <=> " ^ string_of_expr e2 ^ ")"
| Exists (vars, pred) ->
    "(exists " ^
    (String.concat ", " (List.map (fun (name, list) -> name ^ " in " ^ string_of_expr list) vars)) ^
    "where " ^ string_of_expr pred ^ ")"
| Forall (vars, pred) ->
    "(forall " ^
    (String.concat ", " (List.map (fun (name, list) -> name ^ " in " ^ string_of_expr list) vars)) ^
    ", " ^ string_of_expr pred ^ ")"
| Equals (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
| NotEquals (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " != " ^ string_of_expr e2 ^ ")"
| LessThan (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
| LessThanEq (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " <= " ^ string_of_expr e2 ^ ")"
| GreaterThan (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " > " ^ string_of_expr e2 ^ ")"
| GreaterThanEq (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " >= " ^ string_of_expr e2 ^ ")"
| ElementAt (list_expr, index_expr) ->
    string_of_expr list_expr ^ " at " ^ string_of_expr index_expr
| Contains (list_expr, element_expr) ->
    string_of_expr list_expr ^ " contains " ^ string_of_expr element_expr
| Flatten e -> "flatten(" ^ string_of_expr e ^ ")"
| EqualsModOctave (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " is " ^ string_of_expr e2 ^ ")"
| NotEqualsModOctave (e1, e2) ->
    "(" ^ string_of_expr e1 ^ " is-not " ^ string_of_expr e2 ^ ")"
| Range (e1, e2) ->
    "range(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| SymbolicPitchExpr _ 
| SymbolicIntervalExpr _ ->
    "<symbolic expression>"

let string_of_spec_stmt = function
  | RequireStmt e -> "REQUIRE: " ^ string_of_expr e
  | DisallowStmt e -> "DISALLOW: " ^ string_of_expr e
  | PreferStmt (e, w) -> "PREFER: weight " ^ string_of_int w ^ ", " ^ string_of_expr e
  | AvoidStmt (e, w) -> "AVOID: weight " ^ string_of_int w ^ ", " ^ string_of_expr e

let string_of_statement = function
  | ConfigurationStmt (s, _) -> show_configuration_statement s
  | DefinitionStmt (s, _) -> show_definition_statement s
  | SpecificationStmt (s, _) -> string_of_spec_stmt s
  | IncludeStmt (s, _) -> "INCLUDE: " ^ s


