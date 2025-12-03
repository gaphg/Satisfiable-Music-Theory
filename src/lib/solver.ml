open Unix
open Bachend
open Smt_lib_v2_utils

let z3_path = "z3"

(* pre: smt is a smt-lib script without the check-sat/get-model commands
   at the end
   post: None if not sat, Some model if sat
*)
let get_model (smt : string list) : string option =
  let solver_out, solver_in =
    try Unix.open_process_args z3_path [| "-smt2"; "-in" |]
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      raise
        (Failure
           "Z3 not found. Make sure to install Z3 and add it to your PATH.")
  in
  let close_solver () =
    match Unix.close_process (solver_out, solver_in) with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> Printf.eprintf "Solver exited with code %d\n" code
    | Unix.WSIGNALED s -> Printf.eprintf "Solver killed by signal %d\n" s
    | Unix.WSTOPPED s -> Printf.eprintf "Solver stopped by signal %d\n" s
  in
  List.iter
    (fun s ->
      output_string solver_in s;
      output_char solver_in '\n')
    smt;
  (* check if sat *)
  output_string solver_in "(check-sat)\n";
  flush solver_in;
  let sat_str = In_channel.input_line solver_out in
  match sat_str with
  | Some "sat" ->
      output_string solver_in "(get-model)\n";
      close_out solver_in;
      let output = In_channel.input_lines solver_out in
      close_solver ();
      Some (String.concat "\n" output)
  | Some "unsat" ->
      close_solver ();
      None
  | _ -> raise (Failure "unexpected output from solver after (check-sat)")

let solve (smt : string list) (env : dynamic_environment) : int list list option
    =
  match get_model smt with
  | Some model ->
      Some
        (parse_model model
           (Option.get env.voice_count)
           (Option.get env.song_length_units))
  | None -> None
