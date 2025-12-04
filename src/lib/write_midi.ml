open Llama_midi
  

let generate_header (voices) =
  let num_voices = List.length voices in 
  let header = {
    Header.format_ = Format.Simultaneous_tracks num_voices;
    division = Division.Ticks_per_quarter_note 60
  } in
  header

let write_file (voices : 'a list) (f_out : string) =
  let midi_file = {
    Data.header = generate_header(voices);
    tracks = []
  } in
  let chan = File_writer.of_path f_out in
  File_writer.write chan midi_file;

  (* TODO remove print *)
  Printf.printf "%s\n" (Data.to_string midi_file)

