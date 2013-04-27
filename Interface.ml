open Elts
open Simplex

module FloatMatrix = Matrix(Floats) ;;

let explode (s: string) (space: string) : string list =
  let rec to_list (s1: string) (acc: string list) : string list =
    let len = String.length s1 in
    if len = 0 then List.rev acc
    else if len = 1 then List.rev (s1::acc)
    else
      let char = String.sub s 0 1 in
      let s1' = String.sub s 1 len in
      if char = space then to_list s1' acc
      else to_list s1' (char::acc) in
  to_list s []

let rec read_data (chan: in_channel) : Floats.t list list =  
  let row = input_line chan in
  let chars = explode row "," in
  (List.map Floats.from_string chars)::read_data chan


(* Parse command-line arguments. Parses filename *)
let parse_args () : unit =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  let filename =  Sys.argv.(1) in
  try 
    let inchan = open_in filename in 
    let m_list = read_data inchan in
    let matrix = FloatMatrix.from_list m_list in
    (* do simplex stuff here and print *) ()
  with Sys_error ->
    usage ()

parse_args () ;;