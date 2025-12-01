open Llama_midi

let process_file (filename: string) =
  let ic = File_reader.of_path "../../../../examples/Correct4PartHarmony.mid"  (* TODO make it actually use the filename*)
  in let data = File_reader.read ic 
  in Printf.printf "%s\n" (Data.to_string data);;


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
