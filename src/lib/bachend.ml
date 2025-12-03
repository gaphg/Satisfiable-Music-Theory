open Ast
(* open Types *)

let debug = true

type vc_term =
  | Voice of int (* int id of voice *)
  (* matches midi pitch between 0 and 127 (60 is middle C) *)
  | Pitch of int
  (* distance between two pitches, in semitones *)
  | Interval of int * bool option (* interval, up/down direction *)
  (* | ChordVal of int list *)
  | TimeStep of int
  | Integer of int
  | Boolean of bool
  (* indexed via TimeStep *)
  | TimeSeries of (* sz_type *? *) expr list (* element type?, values *)
  | SzList of expr list
  (* logical/symbolic elements for solver *)
  | SymbolicPitch of int * int (* voice id, time step *)
  | SymbolicInterval of vc_term * vc_term (* pitch 1, pitch 2 *)
  | SymbolicEquals of vc_term * vc_term
  | SymbolicAbs of vc_term
  | SymbolicAnd of vc_term list
  | SymbolicOr of vc_term list (* list of predicates/booleans *)
  | SymbolicImplies of vc_term * vc_term
[@@deriving show]

(* functions are not first-order? *)
type func = string list * expr [@@deriving show]
type var_env = (string * vc_term) list [@@deriving show]
type func_env = (string * func) list [@@deriving show]

type dynamic_environment = {
  voices_declared : bool;
  voice_count : int option;
  time_unit_ticks : int option;
      (* default to the gcd of delta times in midi file *)
  song_length_ticks : int option; (* raw number of ticks in midi file *)
  song_length_units : int option; (* song length in terms of time units *)
  key : int option;
  venv : var_env;
  fenv : func_env;
}
[@@deriving show]

let empty_env = {
  voices_declared = false;
  voice_count = None;
  time_unit_ticks = None;
  song_length_ticks = None;
  song_length_units = None;
  key = None;
  venv = [];
  fenv = [];
}
