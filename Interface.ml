open Elts
open Matrix
open MatrixI
open Simplex

(* Creating the Matrix Library module! *)
module EltMatrix : (MATRIX with type elt = Elts.t) =
  MakeMatrix(Elts)

(* Creating the Simplex specific Module *)
module EltSimplex : SIMPLEX = 
  Simplex(Elts)  

(* Testing the Matrix Library *)
let test times = 
  let _ = EltMatrix.run_tests times in
  let _ = Elts.run_tests times in
  print_string "If no errors above, then all tests passed!\n"

(* Parse command-line arguments. Parses filename *)
let parse_args () : unit =
  let usage s = 
    let message = "Possible commands:\n'simplex' {file}\n'run_tests' {times}\n" 
    in
    Printf.printf ("usage: %s arguments.\nError-> %s\n") 
     Sys.argv.(0) s; print_string message; exit 1 in
  if Array.length Sys.argv <> 3 then usage "Incorrect number of arguments.";
  match Sys.argv.(1) with
  | "run tests" -> 
    (try 
      let times = int_of_string Sys.argv.(2) in
      test times
    with
      | Failure s -> usage s)
  | "solve"-> 
    (try 
      let filename =  Sys.argv.(2) in
      let inchan = open_in filename in
      let m = EltSimplex.load inchan in
      (match EltSimplex.solve m with
        | None -> (print_string "No solution exists for:\n"; EltSimplex.print m)
        | Some sol -> (print_string "Solution: "; EltSimplex.print_elt sol)) 
    with
      | Sys_error e -> usage e)
  | _ -> usage "Incorrect inputs."
;;

parse_args () ;;

