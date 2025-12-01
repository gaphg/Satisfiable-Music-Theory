open Unix

let z3_path = "/Users/gaphg/Library/Racket/8.18/pkgs/rosette/bin/z3"

let solve (smt : string list) =
  let (solver_out, solver_in) = Unix.open_process_args z3_path [| "-smt2"; "-in" |] in
  List.iter (fun s -> output_string solver_in s; 
                      output_char solver_in '\n')
            smt;
  close_out solver_in;
  (* TODO: change *)
  let output = (In_channel.input_lines solver_out) in 
  begin
    match Unix.close_process (solver_out, solver_in) with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> Printf.eprintf "Solver exited with code %d\n" code
    | Unix.WSIGNALED s -> Printf.eprintf "Solver killed by signal %d\n" s
    | Unix.WSTOPPED s -> Printf.eprintf "Solver stopped by signal %d\n" s 
  end;
  output