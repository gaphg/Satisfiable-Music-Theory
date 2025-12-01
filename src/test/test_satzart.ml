open Satzart.Ast
open Satzart.Interpreter
open Satzart.Bachend
open Satzart.Solver

let env : dynamic_environment = {
  voices_declared = false;
  voice_count = None;
  time_unit_ticks = None;
  song_length_ticks = None;
  song_length_units = None;
  key = None;
  venv = [];
  fenv = []
}

let program = [
  ConfigurationStmt (VoiceCfgStmt ["soprano"; "alto"; "tenor"; "bass"]);
  ConfigurationStmt (SongLengthUnitsCfgStmt 2);
  (* (DefinitionStmt (FuncDefStmt ("foo", [], Equals (Var "x", BooleanLit true)))); *)
  DefinitionStmt (FuncDefStmt ("isConsonant", ["p1", None; "p2", None; "i", None],
    Contains (ListExpr [Var "i"], IntervalBetween (Var "p1", Var "p2"))
  ));

  DefinitionStmt (ConstDefStmt ("c", IntervalBetween (ElementAt (ListExpr [PitchLit 3], IntegerLit 0), PitchLit 0)));
  (* DefinitionStmt (FuncDefStmt ("suspension", ["v1", None; "v2", None; "t", None],
    BooleanLit true
  )) *)

(*   
  DefinitionStmt (ConstDefStmt ("x", Var "s"));
  (* idea of the require statement: any unbounded variables will have their types inferred
  then the interpreter will range over all possible values of that variable, and output an assert
  statement with each concrete value substituted in. this makes sense because "require" semantically
  means that this predicate is always true
  *)
  (* SpecificationStmt (RequireStmt (Equals (Var "x", Var "v"))); *)
  (* this example req ensures all voices start at the same pitch *)
  SpecificationStmt (RequireStmt (Equals 
    (ElementAt (Pitches (Var "s"), TimeStepLit 0), 
    ElementAt (Pitches (Var "v"), TimeStepLit 0))));

  (* requires tenor[t] = tenor[t+1] *)
  SpecificationStmt (RequireStmt (Equals
    (ElementAt (Pitches (Var "t"), Var "time"),
    ElementAt (Pitches (Var "t"), Plus (Var "time", TimeStepLit 1)))
  )) *)
  (* requires *)
]

let solve_print program = 
  let smt = interpret env program in
  List.iter print_endline smt;
  print_endline "\noutput:";
  List.iter print_endline (solve smt)

let () = 
solve_print program;
(* print_endline "\nnext test\n"; *)
(* solve_print Test_major_scale.program *)
(* require pitches(v1)[0] = C3 *)