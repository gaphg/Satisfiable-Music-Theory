open Satzart.Ast

let program = [
  ConfigurationStmt (VoiceCfgStmt ["soprano"; "alto"; "tenor"; "bass"]);
  ConfigurationStmt (SongLengthUnitsCfgStmt 3);
  (* below might be something that goes in "standard repertoire"? *)
  DefinitionStmt (ConstDefStmt ("Unison", IntervalLit (0, None)));
  DefinitionStmt (ConstDefStmt ("m2", IntervalLit (1, None)));
  DefinitionStmt (ConstDefStmt ("M2", IntervalLit (2, None)));
  DefinitionStmt (ConstDefStmt ("m3", IntervalLit (3, None)));
  DefinitionStmt (ConstDefStmt ("M3", IntervalLit (4, None)));
  DefinitionStmt (ConstDefStmt ("P4", IntervalLit (5, None)));
  DefinitionStmt (ConstDefStmt ("Tritone", IntervalLit (6, None)));
  DefinitionStmt (ConstDefStmt ("P5", IntervalLit (7, None)));
  DefinitionStmt (ConstDefStmt ("m6", IntervalLit (8, None)));
  DefinitionStmt (ConstDefStmt ("M6", IntervalLit (9, None)));
  DefinitionStmt (ConstDefStmt ("m7", IntervalLit (10, None)));
  DefinitionStmt (ConstDefStmt ("M7", IntervalLit (11, None)));
  DefinitionStmt (ConstDefStmt ("Octave", IntervalLit (12, None)));

  DefinitionStmt (FuncDefStmt ("isConsonant", ["p1", None; "p2", None],
    Contains (ListExpr [Var "Unison"; Var "m3"; Var "M3"; Var "P5"; Var "m6"; Var "M6"; Var "Octave"], 
    IntervalBetween (Var "p1", Var "p2"))
  ));
  
  DefinitionStmt (FuncDefStmt ("isDissonant", ["p1", None; "p2", None],
    Contains (ListExpr [Var "m2"; Var "M2"; Var "Tritone"; Var "m7"; Var "M7"], 
    IntervalBetween (Var "p1", Var "p2"))
  ));

  DefinitionStmt (FuncDefStmt ("hasSuspension", ["v1", None; "v2", None; "t", None],
    And (And (
      Equals (ElementAt (Pitches (Var "v1"), Var "t"), ElementAt (Pitches (Var "v1"), Plus (Var "t", TimeStepLit 1))),
      FuncCall ("isConsonant", [ElementAt (Pitches (Var "v1"), Var "t"); ElementAt (Pitches (Var "v2"), Var "t")])),
      FuncCall ("isDissonant", [ElementAt (Pitches (Var "v1"), Plus (Var "t", TimeStepLit 1)); ElementAt (Pitches (Var "v2"), Plus (Var "t", TimeStepLit 1))]))
  ));

  (* e.g. requires all starting pitches to be consonant with each other *)
  SpecificationStmt (RequireStmt 
    (FuncCall ("isConsonant", [ElementAt (Pitches (Var "soprano"), TimeStepLit 0); ElementAt (Pitches (Var "v"), TimeStepLit 0)])));

  (* requires all suspensions to resolve *)
  SpecificationStmt (RequireStmt
    (Implies (
      FuncCall ("hasSuspension", [Var "v1"; Var "v2"; Var "t"]),
      And (
        FuncCall ("isConsonant", [ElementAt (Pitches (Var "v1"), Plus (Var "t", TimeStepLit 2)); ElementAt (Pitches (Var "v2"), Plus (Var "t", TimeStepLit 2))]),
        Or (
          Equals (
            ElementAt (Contour (Var "v1"), Plus (Var "t", TimeStepLit 1)),
            Var "m2"
          ),
          Equals (
            ElementAt (Contour (Var "v1"), Plus (Var "t", TimeStepLit 1)),
            Var "M2"
          )
        )
      )
    )))

]