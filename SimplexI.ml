open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig
 
  val solve : matrix -> elt option

  val load : in_channel -> matrix 

  val run_tests : int -> unit

  val pivot : matrix -> int list * int list -> int -> int list * int list


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
  
  let pivot (mat: matrix) ((non,basic): (int list * int list)) (l:int)
    : (int list * int list) =
    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in

    (* Helper function to find the greatest constraint *)
    let max_index (arr_b : elt array) (arr_c : elt array) : int = 
      let rec index (i:int) (max:int) (max_elt: elt): int = 
	if i < n then
          let curr_elt = Elts.divide arr_c.(i) arr_b.(i) in
 	  (match Elts.compare curr_elt max_elt with
	  | Greater -> index (i+1) i curr_elt
	  | Less | Equal -> index (i+1) max curr_elt) 
	else (* we've reached the end *)
	  max+1 (* matrices are NOT zero indexed *)in
      index 2 1 (Elts.divide arr_c.(1) arr_b.(1)) in

    (* gets our leaving column *)
    let (len1,column) = get_column mat l in

    (* gets our constants column *)
    let (len2,last) = get_column mat p in  
    let _ = assert(p = len1 && p = len2) in

    (* finds the row with the maximum constraint *)
    let row_index = max_index column last in

    (* Finds the entering variable *)
    let rec find_entering (lst: int list) : int option =
      match lst with
      | [] -> None
      | hd::tl -> 
	let elt = get_elt mat (row_index,hd) in
	match Elts.compare elt Elts.one with
	| Equal -> Some hd
	| Less | Greater -> find_entering tl in
    let e =
      match find_entering non with
      | None -> raise (Failure "Could not find entering variable")
      | Some x -> x in

    (* scales our constraint row *)
    let piv = get_elt mat (row_index, l) in
    let _ = scale_row mat row_index (Elts.divide Elts.one piv) in
    
    (* zeros out our leaving column *)
    for i = 1 to n do
      if i <> row_index then
	sub_mult mat i row_index (get_elt mat (i, l))
      else ()
    done;

    (* modify the set *)
    let basic' = e::(List.filter (fun x -> x <> l) basic) in
    let non' = l::(List.filter (fun x -> x <> e) non) in
    (non',basic')
      
end
