open Base
open Stdio
open Constructive_reals
open Calc_core

let rec repl () =
  printf "> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some "quit" | Some "exit" -> ()
  | Some line ->
      (try
         let expr = parse line in
         let result = Evaluator.eval expr in
         printf "%s\n" (eval_to_string result)
       with e -> printf "Error: %s\n" (Exn.to_string e));
      repl ()

let () =
  printf "Constructive Reals Calculator\n";
  printf "Type expressions using +, -, *, /, sin, cos, etc.\n";
  printf
    "Available functions: sin, cos, tan, asin, acos, atan, exp, ln, sqrt, abs, \
     max, min\n";
  printf "Constants: pi, e\n";
  printf "Type 'quit' or 'exit' to exit\n\n";
  repl ()
