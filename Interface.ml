open Elts
open Simplex

module FloatMatrix = Matrix.Matrix(Floats) ;;

let rec read_data (chan: in_channel) : Floats.t list list =  
  try
    let row = input_line chan in
    let chars = Helpers.explode row "," in
    (List.map Floats.from_string chars)::read_data chan
  with End_of_file -> []
;;

(* Parse command-line arguments. Parses filename *)
let parse_args () : unit =
  let usage s = Printf.printf "usage: %s argument.\n Error-> %s\n"
     Sys.argv.(0) s; exit 1 in
  if Array.length Sys.argv <> 2 then usage "Argument number";
  let filename =  Sys.argv.(1) in
  try 
    let inchan = open_in filename in 
    let m_list = read_data inchan in
    let matrix = FloatMatrix.from_list m_list in
    (* do simplex stuff here and print *) FloatMatrix.print matrix
  with
    | Sys_error e -> usage e
;;

parse_args () ;;
