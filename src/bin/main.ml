open Satie

let usage_msg =
  "satie <rules_file> [-midi <midi_file>] [-synth <midi_file>] [-smt2]"

let rules_filename = ref ""
let midi_filename = ref ""
let output_filename = ref ""
let smt2_only = ref false
let anon_fun filename = rules_filename := filename

let speclist =
  [
    ("-midi", Arg.Set_string midi_filename, "Set input midi file");
    ("-synth", Arg.Set_string output_filename, "Set synthesized midi filename");
    ( "-smt2",
      Arg.Set smt2_only,
      "Output an smt-lib script instead of directly solving" );
  ]

let pitch_string_of_int p =
  (List.nth ["C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B"] (p mod 12))
  ^ (string_of_int (p / 12 - 1))

let main (rules_file : string) (input_midi : string option)
    (output_midi : string option) (smt_only : bool) =
  (* 1. parse rules file *)
  let channel = open_in rules_file in
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf rules_file;
  let program = Parser.prog Lexer.tokenize lexbuf in

  if Bachend.debug then List.iter print_endline (List.map Ast.string_of_statement program);

  (* 2. process midi file *)
  let tracks_opt = Option.map Process_midi.process_file input_midi in
  let env, asserts =
    match tracks_opt with
    | Some tracks ->
        ( {
            Bachend.initial_env with
            voice_count = Some (List.length tracks);
            song_length_units = Some (List.length (List.nth tracks 0));
          },
          Smt_lib_v2_utils.asserts_of_tracks tracks )
    | None -> (Bachend.initial_env, [])
  in

  (* translate the program into smt-lib-v2! *)
  let rules_smt, env = Vcgen.translate env program (Option.is_none input_midi) in
  let full_smt = rules_smt @ asserts in

  (* if smt_only, just print smt to stdout *)
  if smt_only then List.iter print_endline full_smt
    (* otherwise, solve and synthesize *)
  else
    let synthesized = Solver.solve full_smt env in
    match (synthesized, input_midi, output_midi) with
    | None, Some im_file, _ ->
        print_endline
          ("music in " ^ im_file ^ " does not satisfy specification in "
         ^ rules_file)
    | None, None, _ ->
        print_endline ("specification in " ^ rules_file ^ " is not satisfiable")
    | Some _, Some im_file, _ ->
        print_endline ("music in " ^ im_file ^ " satisfies specification in " ^ rules_file)
    | Some synth_tracks, None, None ->
        print_endline
          ("specification in " ^ rules_file
         ^ " is satisfied with below assignment");
        synth_tracks
        |> List.iteri (fun v track ->
               print_string ("voice " ^ string_of_int v ^ ":");
               track
               |> List.iter (fun p -> Printf.printf " %4s" (pitch_string_of_int p));
               print_newline ())
    | Some synth_tracks, _, Some fname ->
      Write_midi.write_file synth_tracks fname

let () =
  Arg.parse speclist anon_fun usage_msg;
  let rules_file =
    match !rules_filename with
    | "" -> raise (Arg.Help ".rules file must be provided")
    | _ -> !rules_filename
  in
  let empty_to_none = function "" -> None | s -> Some s in
  main rules_file
    (empty_to_none !midi_filename)
    (empty_to_none !output_filename)
    !smt2_only
