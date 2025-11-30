type expr =
(* literals *)
| PitchLit of int 
| IntervalLit of int
| TimeStepLit of int
| BooleanLit of bool
(* this means in syntax we need a way of differentiating literals for what type they are,
e.g 1p 1i 1t *)

| Var of string
| FuncCall of string * expr list (* function name, arguments *)
| ListExpr of expr list

(* builtin functions on voices *)
| Pitches of expr (* voice -> pitch list *)
| Contour of expr (* voice -> interval list *)
| Diads of expr * expr (* v1, v2 -> interval list *)

(* int/timestep maybe can be extend to pitches/intervals too *)
| PlusExpr of expr * expr
| MinusExpr of expr * expr (* perhaps pitch - pitch gives interval? *)

(* boolean operations *)
| NotExpr of expr
| AndExpr of expr * expr
| OrExpr of expr * expr
| ImpliesExpr of expr * expr
| IffExpr of expr * expr
| IsUnisonPitch of expr * expr  (* pitch, pitch -> bool *)

(* comparison operations *)
| EqualsExpr of expr * expr 
| NotEqualsExpr of expr * expr
| LessThan of expr * expr
| LessThanEq of expr * expr
| GreaterThan of expr * expr * expr
| GreaterThanEq of expr * expr

(* other builtins *)
| ElementAt of expr * expr (* list, index -> element *)
[@@deriving show]

type configuration_statement =
| VoiceCfgStmt of string list
| TimeUnitTicksCfgStmt of int
| SongLengthUnitsCfgStmt of int
| KeyCfgStmt of expr (* type: pitch *)

type definition_statement = 
(* const name, const definition *)
| ConstDefStmt of string * expr 
(* function name, function arguments, function body *)
| FuncDefStmt of string * string list * expr
(* "expr" below should be type predicate *)

type specification_statement = 
| RequireStmt of expr
| DisallowStmt of expr
| PreferStmt of expr
| AvoidStmt of expr

type statement = 
| ConfigurationStmt of configuration_statement
| DefinitionStmt of definition_statement
| SpecificationStmt of specification_statement

type program = statement list

let string_of_expr (e : expr) =
  match e with
  | Var name -> name
  | _ -> "<not yet implemented>"