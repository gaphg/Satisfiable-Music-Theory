open Satzart

let usage_msg = 
  "satzart <rules_file> [-midi <midi_file>] [-synth <midi_file>] [-smt]"
let rules_filename = ref ""
let midi_filename = ref ""
let output_filename = ref ""
let smt_only = ref false

let anon_input_set filename = rules_filename := filename

let speclist = 
  [
    ("-midi", Arg.Set_string midi_filename, "Set input midi file");
    ("-synth", Arg.Set_string output_filename, "Set synthesized midi filename");
    ("-smt", Arg.Set smt_only, "Output an smt-lib script instead of directly solving");
  ]

let main (rules_file : string)
         (input_midi : string)
         (output_midi : string)
         (smt_only : bool) =
  (* TODO: change below to instead parse rules file into program *)
  (* 1. parse rules file *)
  let channel = open_in rules_file in
  let lexbuf = Lexing.from_channel channel in
  let program = Parser.prog Lexer.tokenize lexbuf in 
  let tracks_opt = if !midi_filename = ""
    then None
  else Some (Process_midi.process_file input_midi) in
  let env, asserts = match tracks_opt with
  | Some tracks -> 
    {Bachend.empty_env with
      voice_count = Some (List.length tracks);
      song_length_units = Some (List.length (List.nth tracks 0));
    }, Smt_lib_v2_utils.asserts_of_tracks tracks
  | None -> Bachend.empty_env, []
  in
  (* interpret the program! *)
  let rules_smt, env = Interpreter.interpret env program in
  let full_smt = rules_smt @ asserts in

  (* if smt_only, just print smt to stdout *)
  if smt_only then List.iter print_endline full_smt

  (* otherwise, solve and synthesize *)
  else let synthesized = Solver.solve full_smt env in
  match synthesized, tracks_opt with
  | None, Some _ -> 
    print_endline ("music in " ^ input_midi ^ " does not satisfy specification in " ^ rules_file)
  | None, None -> print_endline ("specification in " ^ rules_file ^ " is not satisfiable")
  | Some synth_tracks, _ ->
  (* TODO: change this *)
  print_endline ("specification in " ^ rules_file ^ " is satisfied; output to " ^ output_midi);
  synth_tracks |>
  List.iteri (fun v track ->
    print_string ("voice " ^ (string_of_int v) ^ ":");
    track |> List.iter (fun p -> print_string (" " ^ (string_of_int p)));
    print_newline ()
    )

let () = 
Arg.parse speclist anon_input_set usage_msg;
main !rules_filename !midi_filename !output_filename !smt_only
