open Matrix
open EltMatrix

module type SIMPLEX =
sig

  val solve : matrix -> elt option

  val load : in_channel -> matrix 

  val run_tests : int -> unit

end 

module Simplex: SIMPLEX =
struct

  let rec read_data (chan: in_channel) : elt list list =  
    try
      let row = input_line chan in
      let chars = Helpers.explode row "," in
      (List.map from_string_elt chars)::read_data chan
    with End_of_file -> []
  ;;

  let load (chan: in_channel): matrix =
    let m_list = read_data chan in
    let matrix = from_list m_list in
    matrix

  let solve (m: matrix) : elt option = None

  let run_tests times = ()

end
