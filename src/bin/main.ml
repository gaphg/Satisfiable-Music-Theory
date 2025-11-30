(* this is just me playing around with z3 stuff. look in test_satzart.ml for testing the ast *)

let z3_path = "/Users/gaphg/Library/Racket/8.18/pkgs/rosette/bin/z3"

let smt_script = "
(set-logic QF_BV)
(declare-const p (_ BitVec 7))
(assert (= p (_ bv9 7)))
(assert (bvult p (_ bv10 7)))
(check-sat)
(get-model)\n
"

let (solver_out, solver_in) = Unix.open_process_args z3_path [| "-smt2"; "-in" |]

let () = 
output_string solver_in smt_script; 
close_out solver_in;
let lines = (In_channel.input_lines solver_out) in List.iter print_endline lines;
match Unix.close_process (solver_out, solver_in) with
| Unix.WEXITED 0 -> ()
| Unix.WEXITED code -> Printf.eprintf "Solver exited with code %d\n" code
| Unix.WSIGNALED s -> Printf.eprintf "Solver killed by signal %d\n" s
| Unix.WSTOPPED s -> Printf.eprintf "Solver stopped by signal %d\n" s;