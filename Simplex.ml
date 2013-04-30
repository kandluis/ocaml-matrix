module type SIMPLEX =
sig

  type elt

  type matrix

  val solve : matrix -> elt option

  val load : in_channel -> matrix 

  val print : matrix -> unit

  val print_elt : elt -> unit

end 

module MakeSimplex(C: EltsI.ORDERED_AND_OPERATIONAL): SIMPLEX =
struct

  module M : (MatrixI.SIMPLEXMATRIX with type elt = C.t) =
	Matrix.MakeMatrix(C)

  type elt = C.t

  type matrix = M.matrix

  let print = M.print

  let print_elt = C.print

  let rec read_data (chan: in_channel) : elt list list =  
    try
      let row = input_line chan in
      let chars = Helpers.explode row "," in
      (List.map C.from_string chars)::read_data chan
    with End_of_file -> []
  ;;

  let load (chan: in_channel): matrix =
    let m_list = read_data chan in
    let matrix = M.from_list m_list in
    matrix

  let solve (m: matrix) : elt option = None

end
