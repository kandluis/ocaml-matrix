open Matrix
open EltMatrix

module type SIMPLEX =
sig
  
  val m : int 
  val n : int

  val solve : matrix -> elt option

  val load : in_channel -> matrix 

  val run_tests : int -> unit


end 

module Simplex: SIMPLEX =
struct
  (*
  let m = 
  let n = 
  *)
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
  
  let pivot (A: matrix) ((N,B): (int list * int list)) (l:int)
    : (int list * int list) =
    let (n,p) = get_dimensions A in 
    let (l,column) = get_column A l in 
    let (l1, last) = get_column A p in  
    let _ = assert(p = l) in 
    
    let index = 
    
        

 (*
    let A': matrix = EltMatrix.empty m n in 
    let get_A = Eltmatrix.get_elt A in  
    get_A' = EltMatrix.get_elt A' in 
    let b'_e : float = b.(l) /. (EltMatrix.get_elt A (l,e)) in 
    List.iter 
      (fun j -> if j <> e then 
                  EltMatrix.set_elt A' (e,j) ((get_A (l,j)) /. (get_A (l, e)) ))
      N; *)
    
    
end
