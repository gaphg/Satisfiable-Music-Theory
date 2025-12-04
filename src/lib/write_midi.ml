open Llama_midi

(* constants for assuming note lengths *)
let ticks_per_quarter = 480
let velocity = 80
let whole_note_length = 4 * ticks_per_quarter - 1

(* generates the note on message *)
let gen_note_event (voice_num : int) (pitch : int) (vel : int) =
  let msg = Channel_voice_message.Note_on { note = pitch; velocity = vel } in
  Message.Channel_voice_message { channel = voice_num; message = msg }

(* generates the event *)
let gen_event (time : int) (message : Message.t) : Event.t =
  Event.{ delta_time = time; message = message }

(* generate note messages *)
let gen_play_note (voice_num : int) (pitch : int) =
  [gen_event 0 (gen_note_event voice_num pitch velocity);
   gen_event whole_note_length (gen_note_event voice_num pitch 0)]

(* generates the end of track message *)
let gen_end_of_track =
  [gen_event 0 (Message.Meta_event Meta_event.End_of_track)]

(* generates the track for a certain voice, given the list of pitches *)
let generate_track (voice_num : int) (vals : int list) : Track.t =
  let notes = List.concat_map (gen_play_note voice_num) vals in
  notes @ gen_end_of_track

(* generates the midi header *)
let generate_header (voices) =
  let num_voices = List.length voices in 
  let header = {
    Header.format_ = Format.Simultaneous_tracks num_voices;
    division = Division.Ticks_per_quarter_note ticks_per_quarter
  } in
  header

let write_file (voices : int list list) (f_out : string) =
  let midi_file = {
    Data.header = generate_header(voices);
    tracks = List.mapi generate_track voices
  } in
  let chan = File_writer.of_path f_out in
  File_writer.write chan midi_file
