open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig
 
  val solve : matrix -> elt option
  
  val run_tests : int -> unit

  val pivot : matrix -> int list * int list -> int -> int list * int list


end 

module Simplex: SIMPLEX =
struct

  let solve (m: matrix) : elt option = None

  let run_tests times = ()
  
  let pivot (mat: matrix) ((non,basic): (int list * int list)) (l:int)
    : (int list * int list) =
    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in

    (* Helper function to find the greatest constraint *)
    let min_index (arr_b : elt array) (arr_c : elt array) : int = 
      let rec index (i:int) (min:int) (min_elt: elt): int = 
	if i < n then
          let curr_elt = Elts.divide arr_c.(i) arr_b.(i) in
 	  (match Elts.compare curr_elt min_elt with
	  | Less -> index (i+1) i curr_elt
	  | Greater | Equal -> index (i+1) min curr_elt) 
	else (* we've reached the end *)
	  min+1 (* matrices are NOT zero indexed *)in
      index 2 1 (Elts.divide arr_c.(1) arr_b.(1)) in

    (* gets our leaving column *)
    let (len1,column) = get_column mat l in

    (* gets our constants column *)
    let (len2,last) = get_column mat p in  
    let _ = assert(n = len1) in
    let _ = assert(n = len2) in

    (* finds the row with the maximum constraint *)
    let row_index = min_index column last in

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
      match find_entering basic with
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
    let basic' = l::(List.filter (fun x -> x <> e) basic) in
    let non' = e::(List.filter (fun x -> x <> l) non) in
    (non',basic')
      
end
