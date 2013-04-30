open Elts
open Matrix
open MatrixI
open Simplex

(* Creating the Matrix Library module! *)
module FloatMatrix : (MATRIX with type elt = Floats.t) =
  MakeMatrix(Floats)

(* Testing the Matrix Library *)
let test times = 
  let _ = FloatMatrix.run_tests times in
  let _ = Floats.run_tests times in
  print_string "If no erros, then all tests passed!\n"

(* Creating the Simplex specific Module *)
module FloatSimplex : SIMPLEX = 
  Simplex(Floats)  


let rec read_data (chan: in_channel) : Floats.t list list =  
  try
    let row = input_line chan in
    let chars = Helpers.explode row "," in
    (List.map Floats.from_string chars)::read_data chan
  with End_of_file -> []
;;

(* Parse command-line arguments. Parses filename *)
let parse_args () : unit =
  let usage s = 
    let message = "Possible commands:\n'simplex' {file}\n'run_tests' {times}\n" 
    in
    Printf.printf ("usage: %s arguments.\nError-> %s\n") 
     Sys.argv.(0) s; print_string message; exit 1 in
  if Array.length Sys.argv <> 3 then usage "Incorrect number of arguments.";
  match Sys.argv.(1) with
  | "run_tests" -> 
    (try 
      let times = int_of_string Sys.argv.(2) in
      test times
    with
      | Failure s -> usage s)
  | "simplex"->  
    (try 
      let filename =  Sys.argv.(2) in
      let inchan = open_in filename in 
      let m_list = read_data inchan in
      let matrix = FloatMatrix.from_list m_list in
      (* do simplex stuff here and print *) FloatMatrix.print matrix
    with
      | Sys_error e -> usage e)
  | _ -> usage "Incorrect inputs."
;;

parse_args () ;;

