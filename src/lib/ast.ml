open Types

type expr =
(* literals *)
| PitchLit of int 
| IntervalLit of int * bool option (* user may specify direction, true -> ascending *)
| TimeStepLit of int
| BooleanLit of bool
| IntegerLit of int
| DirectionLit of bool (* only can specify concrete direction, not unspecified *)
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

| IntervalBetween of expr * expr (* p1, p2 -> interval *)

(* int/timestep maybe can be extend to pitches/intervals too *)
| Plus of expr * expr
| Minus of expr * expr (* perhaps pitch - pitch gives interval? *)

(* boolean operations *)
| Not of expr
| And of expr * expr
| Or of expr * expr
| Implies of expr * expr
| Iff of expr * expr

(* comparison operations *)
| Equals of expr * expr 
| NotEquals of expr * expr
| LessThan of expr * expr
| LessThanEq of expr * expr
| GreaterThan of expr * expr * expr
| GreaterThanEq of expr * expr

(* other builtins *)
| ElementAt of expr * expr (* list, index -> element *)
| Contains of expr * expr (* list, element -> bool *)
| EqualsModuloOctaveExpr of expr * expr  (* pitch, pitch -> bool *)

(* for internal implementation *)
| SymbolicPitchExpr of int * int
| SymbolicIntervalExpr of (int * int) * (int * int)
[@@deriving show]

type configuration_statement =
| VoiceCfgStmt of string list
| TimeUnitTicksCfgStmt of int
| SongLengthUnitsCfgStmt of int
| KeyCfgStmt of expr (* type: pitch *)

type definition_statement = 
(* const name, const definition *)
| ConstDefStmt of string * expr 
(* function name, function arguments/type annotations, function body *)
| FuncDefStmt of string * (string * sz_type option) list * expr
(* "expr" below should be type predicate *)

type specification_statement = 
| RequireStmt of expr
| DisallowStmt of expr
| PreferStmt of expr
| AvoidStmt of expr
[@@deriving show]

type statement = 
| ConfigurationStmt of configuration_statement
| DefinitionStmt of definition_statement
| SpecificationStmt of specification_statement

type program = statement list

let string_of_expr = show_expr (* TODO: change *)
  (* match e with
  | Var name -> name
  | _ -> "<not yet implemented>" *)

  (* TODO: change *)
let string_of_spec_stmt =
  show_specification_statement