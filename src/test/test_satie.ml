(* open Satie.Ast
open Satie.Vcgen
open Satie.Bachend
open Satie.Solver
open Satie.Smt_lib_v2_utils
open Satie.Process_midi
open Satie.Parser
open Satie.Lexer
open Satie
open Satie.Write_midi

let env : dynamic_environment =
  {
    voices_declared = false;
    voice_count = None;
    time_unit_ticks = None;
    song_length_ticks = None;
    song_length_units = None;
    key = None;
    venv = [];
    fenv = [];
  }

let program =
  [
    ConfigurationStmt (VoiceCfgStmt [ "soprano"; "alto"; "tenor"; "bass" ]);
    ConfigurationStmt (SongLengthUnitsCfgStmt 7);
    SpecificationStmt
      (RequireStmt
         (Equals (ElementAt (Pitches (Var "bass"), TimeStepLit 0), PitchLit 48)));
    (* (DefinitionStmt (FuncDefStmt ("foo", [], Equals (Var "x", BooleanLit true)))); *)
    (* DefinitionStmt (FuncDefStmt ("isConsonant", ["p1", None; "p2", None; "i", None],
         Contains (ListExpr [Var "i"], IntervalBetween (Var "p1", Var "p2"))
       ));

       DefinitionStmt (ConstDefStmt ("c", IntervalBetween (ElementAt (ListExpr [PitchLit 3], IntegerLit 0), PitchLit 0))); *)

    (* DefinitionStmt (FuncDefStmt ("suspension", ["v1", None; "v2", None; "t", None],
         BooleanLit true
       )) *)
    DefinitionStmt (ConstDefStmt ("x", Var "soprano"));
    (* idea of the require statement: any unbounded variables will have their types inferred
       then the interpreter will range over all possible values of that variable, and output an assert
       statement with each concrete value substituted in. this makes sense because "require" semantically
       means that this predicate is always true
    *)
    (* SpecificationStmt (RequireStmt (Equals (Var "x", Var "v"))); *)
    (* this example req ensures all voices start at the same pitch *)
    (* SpecificationStmt (RequireStmt (Equals
         (ElementAt (Pitches (Var "soprano"), TimeStepLit 0),
         ElementAt (Pitches (Var "v"), TimeStepLit 0))));

       (* requires tenor[t] = tenor[t+1] *)
       SpecificationStmt (RequireStmt (Equals
         (ElementAt (Pitches (Var "tenor"), Var "time"),
         ElementAt (Pitches (Var "tenor"), Plus (Var "time", TimeStepLit 1)))
       )); *)

    (* SpecificationStmt (RequireStmt (Equals
        (ElementAt (Pitches (Var "bass"), TimeStepLit 0), PitchLit 60)));
       SpecificationStmt (RequireStmt (Equals
        (ElementAt (Pitches (Var "tenor"), TimeStepLit 1), PitchLit 62))); *)
    (* requires *)
  ]
(*
   let process_program program =
     let filename = "../../../../examples/Correct4PartHarmony.mid" in
     let tracks = process_file filename in
     let asserts = asserts_of_tracks tracks in
     let env =
       {
         voices_declared = false;
         voice_count = Some (List.length tracks);
         time_unit_ticks = None;
         song_length_ticks = None;
         song_length_units = Some (List.length (List.nth tracks 0));
         key = None;
         venv = [];
         fenv = [];
       }
     in
     let smt, _ = interpret env program in
     let full_smt = smt @ asserts in

     List.iter print_endline full_smt;

     let output = get_model full_smt in

     print_endline (Option.get output);
     ()

   let solve_print program =
     let smt, _ = interpret env program in
     List.iter print_endline smt;
     print_endline "\noutput:";
     List.iter print_endline (Option.get (get_model smt)) *)

let string_of_token = function
  | Satie.Parser.EOF -> "EOF"
  | Satie.Parser.VOICE_DECL -> "VOICE_DECL"
  | Satie.Parser.TIME_UNIT_DECL -> "TIME_UNIT_DECL"
  | Satie.Parser.MEASURE_DECL -> "MEASURE_DECL"
  | Satie.Parser.KEY_DECL -> "KEY_DECL"
  | Satie.Parser.REQUIRE -> "REQUIRE"
  | Satie.Parser.DISALLOW -> "DISALLOW"
  | Satie.Parser.PREFER -> "PREFER"
  | Satie.Parser.AVOID -> "AVOID"
  | Satie.Parser.LPAREN -> "LPAREN"
  | Satie.Parser.RPAREN -> "RPAREN"
  | Satie.Parser.PITCHES -> "PITCHES"
  | Satie.Parser.CONTOUR -> "CONTOUR"
  | Satie.Parser.DIADS -> "DIADS"
  | Satie.Parser.INTERVAL -> "INTERVAL"
  | Satie.Parser.PLUS -> "PLUS"
  | Satie.Parser.MINUS -> "MINUS"
  | Satie.Parser.NOT -> "NOT"
  | Satie.Parser.AND -> "AND"
  | Satie.Parser.OR -> "OR"
  | Satie.Parser.IMPLIES -> "IMPLIES"
  | Satie.Parser.IFF -> "IFF"
  | Satie.Parser.EQUALS -> "EQUALS"
  | Satie.Parser.NOT_EQUALS -> "NOT_EQUALS"
  | Satie.Parser.LESS -> "LESS"
  | Satie.Parser.LEQ -> "LEQ"
  | Satie.Parser.GREATER -> "GREATER"
  | Satie.Parser.GEQ -> "GEQ"
  | Satie.Parser.AT -> "AT"
  | Satie.Parser.CONTAINS -> "CONTAINS"
  | Satie.Parser.IS -> "IS"
  | Satie.Parser.FLATTEN -> "FLATTEN"
  | Satie.Parser.VOICE_TYPE -> "VOICE_TYPE"
  | Satie.Parser.PITCH_TYPE -> "PITCH_TYPE"
  | Satie.Parser.INTERVAL_TYPE -> "INTERVAL_TYPE"
  | Satie.Parser.TIMESTEP_TYPE -> "TIMESTEP_TYPE"
  | Satie.Parser.BOOLEAN_TYPE -> "BOOLEAN_TYPE"
  | Satie.Parser.TIMESERIES_TYPE -> "TIMESERIES_TYPE"
  | Satie.Parser.INTEGER_TYPE -> "INTEGER_TYPE"
  | Satie.Parser.OF -> "OF"
  | Satie.Parser.DEFINE -> "DEFINE"
  | Satie.Parser.COMMA -> "COMMA"
  | Satie.Parser.LIST_TYPE -> "LIST_TYPE"
  | Satie.Parser.TRUE_TOKEN -> "TRUE_TOKEN"
  | Satie.Parser.FALSE_TOKEN -> "FALSE_TOKEN"
  | Satie.Parser.ID s -> "ID(" ^ s ^ ")"
  | Satie.Parser.INTLIT n -> "INTLIT(" ^ string_of_int n ^ ")"
  | Satie.Parser.LBRACK -> "LBRACK"
  | Satie.Parser.RBRACK -> "RBRACK"
  | Satie.Parser.PITCHLIT p -> "PITCHLIT(" ^ string_of_int p ^ ")"
  | Satie.Parser.INTERVALLIT p -> "INTERVALLIT(" ^ string_of_int p ^ ")"
  | Satie.Parser.TIMESTEPLIT p -> "TIMESTEPLIT(" ^ string_of_int p ^ ")"
  | Satie.Parser.IS_NOT   -> "IS_NOT"
  | Satie.Parser.UP -> "UP"
  | Satie.Parser.DOWN -> "DOWN"
  | Satie.Parser.IN -> "IN"
  | Satie.Parser.EXISTS -> "EXISTS"
  | Satie.Parser.WHERE -> "WHERE"
  | Satie.Parser.FORALL -> "FORALL"
  | Satie.Parser.INCLUDE -> "INCLUDE"
  | Satie.Parser.FILENAME f -> "FILENAME(" ^ f ^ ")"
  | Satie.Parser.WEIGHT -> "WEIGHT"
 
let () =
  let rec print_tokens lexbuf =
    let token = Satie.Lexer.tokenize lexbuf in
    print_endline (string_of_token token);
    if token <> Satie.Parser.EOF then print_tokens lexbuf
  in

  let filename = "../../../../example_rules/bach4part.rules" in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  print_tokens lexbuf;
  let results = prog tokenize lexbuf in
  Printf.printf "%s\n" (Satie.Ast.show_program results);

  close_in chan

(* let () =
   solve_print Test_major_scale.program; *)

(* let print_int_list lst =
     List.iter (fun x -> Printf.printf "%d " x) lst;
     print_newline ()
   in
   let print_int_list_list lsts =
     List.iteri (fun i lst ->
       Printf.printf "Track %d: " i;
       print_int_list lst
     ) lsts
   in
   let tracks = process_file "../../../../examples/Correct4PartHarmony.mid"
   in
   print_int_list_list tracks;
   let asserts = process_piece tracks in
   List.iter print_endline asserts *)

(* let () =
  let voices = [[60; 62; 64; 65; 67; 69; 71; 72]; [60; 60; 60; 60; 60; 60; 60; 60]] in
  let fname = "xxxx.mid" in
  write_file voices fname; *)
    
  process_file "../../../../examples/Correct4PartHarmony.mid";
*)