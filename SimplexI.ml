open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig

  type system

  val make_system : matrix -> (int list * int list) -> system

  val break_system : system -> matrix * (int list * int list)

  val load : string -> system
 
  val solve : matrix -> elt option
  
  val run_tests : int -> unit

end 

module Simplex: SIMPLEX =
struct

  type system = matrix * (int list * int list)

  let make_system (m: matrix) ((lst1, lst2): int list * int list) : system =
   let s = m,(lst1,lst2) in s

  let break_system (s: system) : matrix * (int list * int list) =
    let (m,(lst1,lst2)) = s in (m,(lst1,lst2)) 

  let solve (m: matrix) : elt option = None
  
  let pivot (s: system) (l:int) : system =
    (* extracting information from the system *)
    let (mat,(non,basic)) = break_system s in

    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in

    (* Helper function to find the greatest constraint *)
    let min_index (arr_b : elt array) (arr_c : elt array) : int = 
      let rec index (i:int) (min:int) (min_elt: elt option): int = 
        if i < n then
          match Elts.compare arr_b.(i) Elts.zero with
          | Less | Equal -> index (i+1) min min_elt  
          | Greater ->
            let curr_div = Elts.divide arr_c.(i) arr_b.(i) in
            match min_elt with
            | None -> index (i+1) i (Some curr_div)
            | Some prev_div ->
              match Elts.compare curr_div prev_div with
              | Less  -> index (i+1) i (Some curr_div)
              | Equal | Greater -> index (i+1) min min_elt 
        else (* we've reached the end *)
          min+1 (* matrices are NOT zero indexed *)in
      match index 1 0 None with 
      | 1 -> raise (Failure "Could not find min_index.")
      | i -> i in

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
    (mat,(non',basic'))

  let run_tests times = ()
      
end
