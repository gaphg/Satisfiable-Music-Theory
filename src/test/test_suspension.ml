open Satzart.Ast

let program = [
  ConfigurationStmt (VoiceCfgStmt ["soprano"; "alto"; "tenor"; "bass"]);
  ConfigurationStmt (SongLengthUnitsCfgStmt 2);

  DefinitionStmt (ConstDefStmt ("Unison", IntervalLit (0, None)));
  DefinitionStmt (ConstDefStmt ("m3", IntervalLit (3, None)));
  DefinitionStmt (ConstDefStmt ("M3", IntervalLit (4, None)));
  DefinitionStmt (ConstDefStmt ("P5", IntervalLit (7, None)));
  DefinitionStmt (ConstDefStmt ("m6", IntervalLit (8, None)));
  DefinitionStmt (ConstDefStmt ("M6", IntervalLit (9, None)));

  DefinitionStmt (FuncDefStmt ("isConsonant", ["p1", None; "p2", None],
    Contains (ListExpr [Var "Unison"; Var "m3"; Var "M3"; Var "P5"; Var "m6"; Var "M6"], 
    IntervalBetween (Var "p1", Var "p2"))
  ));

  (* requires all starting pitches to be consonant with each other *)
  SpecificationStmt (RequireStmt 
    (FuncCall ("isConsonant", [ElementAt (Pitches (Var "soprano"), TimeStepLit 0); ElementAt (Pitches (Var "v"), TimeStepLit 0)])))

]