open Satzart.Ast

let program =
  [
    ConfigurationStmt (VoiceCfgStmt [ "v" ]);
    ConfigurationStmt (SongLengthUnitsCfgStmt 8);
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 0), IntervalLit (2, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 1), IntervalLit (2, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 2), IntervalLit (1, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 3), IntervalLit (2, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 4), IntervalLit (2, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 5), IntervalLit (2, None))));
    SpecificationStmt
      (RequireStmt
         (Equals
            (ElementAt (Contour (Var "v"), TimeStepLit 6), IntervalLit (1, None))));
  ]
