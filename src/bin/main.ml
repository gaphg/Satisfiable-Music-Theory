open Satzart

let usage_msg = 
  "satzart <rules_file> [-midi <midi_file>] [-synth <midi_file>] [-smt]"
let rules_filename = ref ""
let midi_filename = ref ""
let output_filename = ref ""
let produce_smt = ref false

let anon_input_set filename = rules_filename := filename

let speclist = 
  [
    ("-midi", Arg.Set_string midi_filename, "Set input midi file");
    ("-synth", Arg.Set_string output_filename, "Set synthesized midi filename");
    ("-smt", Arg.Set produce_smt, "Output an smt-lib script instead of directly solving");
  ]

let main (rules_file : string)
         (input_midi : string)
         (output_midi : string)
         (produce_smt : bool) =
  (* TODO: change below to instead parse rules file into program *)
  let program = [] in
  let tracks_opt = if !midi_filename = ""
    then None
  else Some (Process_midi.process_file input_midi) in
  let env, asserts = match tracks_opt with
  | Some tracks -> 
    {Bachend.empty_env with
      voice_count = Some (List.length tracks);
      song_length_units = Some (List.length (List.nth tracks 0));
    }, Smt_lib.asserts_of_tracks tracks
  | None -> Bachend.empty_env, []
  in
  (* interpret the program! *)
  let smt = Interpreter.interpret env program @ asserts in
  let output = Solver.solve smt in
  List.iter (fun s -> print_endline s; print_newline ()) output

let () = 
Arg.parse speclist anon_input_set usage_msg;
main !rules_filename !midi_filename !output_filename !produce_smt
