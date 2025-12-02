open Satzart.Ast
open Satzart.Interpreter
open Satzart.Bachend
open Satzart.Solver
open Satzart.Smt_lib_v2_utils
open Satzart.Process_midi
open Satzart.Parser
open Satzart.Lexer
open Satzart

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
  print_endline (Option.get (get_model smt))

(* let () =
  let filename = "../../../../example_rules/test.txt" in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  let rec collect acc =
    try
      let result = Satzart.Parser.main Satzart.Lexer.token lexbuf in
      collect (result::acc)
    with
    | Satzart.Lexer.Eof -> List.rev acc
  in

  let results = collect [] in
  List.iter (fun r -> Printf.printf "Parsed result: %s\n" r) results;

  close_in chan *)
let model = "(
  (define-fun v0t6 () (_ BitVec 8)
    #x3c)
  (define-fun v0t7 () (_ BitVec 8)
    #x3b)
  (define-fun v0t5 () (_ BitVec 8)
    #x3a)
  (define-fun v0t2 () (_ BitVec 8)
    #x37)
  (define-fun v0t3 () (_ BitVec 8)
    #x36)
  (define-fun v0t0 () (_ BitVec 8)
    #x33)
  (define-fun v0t1 () (_ BitVec 8)
    #x35)
  (define-fun v0t4 () (_ BitVec 8)
    #x38)
)"

let whitespace = Re.(alt [space; char '\n'])
let match_model = Re.(
  compile (seq [
        str "(define-fun ";
        char 'v';
        group ~name:"voice_id" (rep1 digit);
        char 't';
        group ~name:"time_step" (rep1 digit);
        str " () (_ BitVec 8)"; rep whitespace;
        str "#x"; group ~name:"pitch" (seq [xdigit; xdigit]); char ')'
      ]))
     


let () = (Re.all match_model model) 
|> List.map Re.Group.all
|> List.iter (Array.iter print_endline)

(* let () = solve_print Test_major_scale.program; *)

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
