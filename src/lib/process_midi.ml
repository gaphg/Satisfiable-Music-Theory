open Llama_midi

(* processes an event, assuming we only care about Note_ons (with nonzero velocity) *)
let process_event (e : Event.t) =
  match e.message with
  | Channel_voice_message cvm -> (
      match cvm.message with
      | Note_on note_ev ->
          if note_ev.velocity = 0 then None else Some note_ev.note
      | _ -> None)
  | _ -> None

(* gets a list of the MIDI pitches from a track *)
let process_track (t : Track.t) =
  List.rev
    (List.fold_left
       (fun acc e ->
         match process_event e with Some note -> note :: acc | None -> acc)
       [] t)

(* turns a midi file into a list of its pitches 
 * pitches are ints, where 60 = middle C, following midi standard *)
let process_file (filename : string) : int list list =
  let ic = File_reader.of_path filename in
  let data = File_reader.read ic in
  let tracks = data.tracks in List.map process_track tracks
