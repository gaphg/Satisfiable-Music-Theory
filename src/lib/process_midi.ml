open Llama_midi
open Errors

(* returns the number of voices *)
let process_header (h: Header.t) =
  match h.format_ with
  | Format.Single_track           -> 1
  | Format.Simultaneous_tracks n  -> n
  | Format.Sequential_tracks n    -> raise (UnsupportedMidiFormat "Sequential tracks not supported")

(* processes an event, assuming we only care about Note_ons (with nonzero velocity) *)
let process_event (e: Event.t) = 
  match e.message with
  | Channel_voice_message cvm   -> 
    (match cvm.message with 
    | Note_on note_ev -> if note_ev.velocity == 0 then None else Some note_ev.note
    | _               -> None)
  | _                           -> None

(* gets a list of the MIDI pitches from a track *)
let process_track (t: Track.t) = 
  List.rev 
    (List.fold_left (fun acc e ->
      match process_event e with
      | Some note  -> note::acc
      | None       -> acc
    ) [] t)

let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;
  print_newline ()


let process_file (filename: string) =
  let ic = File_reader.of_path "../../../../examples/Correct4PartHarmony.mid"  (* TODO make it actually use the filename*)
  in let data = File_reader.read ic 
  (* in let num_voices = process_header data.header *)
  in let tracks = data.tracks
  in List.map process_track tracks;;

  (* in List.iter (fun track ->
    print_int_list (process_track track)
  ) tracks *)

  (* in print_int_list (process_track (List.nth tracks 0)) *)
  (* in Printf.printf "++++++ NUMBRE OF VOICES %i \n %s\n" num_voices (Track.to_string (List.nth tracks 0));; *)



(* let ic = File_reader.of_path "../../examples/Correct4PartHarmony.mid" in  (* TODO make it actually use the filename*)
let data = File_reader.read ic in
Printf.printf "%s\n" (Llama.Data.to_string data);; *)

  (* Printf.printf "%s\n" "hi"

let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else failwith "Usage: process_midi <filename>"
  in
  let ic = File_reader.of_path filename in
  let data = File_reader.read ic in
  Printf.printf "%s\n" (Data.to_string data) *)
