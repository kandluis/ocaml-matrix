open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig

  type system

  val make_system : matrix -> (int list * int list) -> system

  val break_system : system -> matrix * (int list * int list)

  (*val load_system : string -> system*)
 
  val solve : system -> elt option
  
  val run_tests : int -> unit

end 

module Simplex: SIMPLEX =
struct

  type system = matrix * (int list * int list)

  let make_system (m: matrix) ((lst1, lst2): int list * int list) : system =
   let s = m,(lst1,lst2) in s

  let break_system (s: system) : matrix * (int list * int list) =
    let (m,(lst1,lst2)) = s in (m,(lst1,lst2)) 
  
  let pivot (s: system) (e:int) : system =
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

    (* gets our entering column *)
    let (len1,column) = get_column mat e in

    (* gets our constants column *)
    let (len2,last) = get_column mat p in  
    let _ = assert(n = len1) in
    let _ = assert(n = len2) in

    (* finds the row with the maximum constraint *)
    let row_index = min_index column last in

    (* Finds the leaving variable *)
    let rec find_leaving (lst: int list) : int option =
      match lst with
      | [] -> None
      | hd::tl -> 
        let elt = get_elt mat (row_index,hd) in
        match Elts.compare elt Elts.one with
        | Equal -> Some hd
        | Less | Greater -> find_leaving tl in
    let l =
      match find_leaving basic with
      | None -> raise (Failure "Could not find entering variable")
      | Some x -> x in

    (* scales our constraint row *)
    let piv = get_elt mat (row_index, e) in
    let _ = scale_row mat row_index (Elts.divide Elts.one piv) in
    
    (* zeros out our entering column *)
    for i = 1 to n do
      if i <> row_index then
  sub_mult mat i row_index (get_elt mat (i, e))
      else ()
    done;

    (* modify the set *)
    let basic' = e::(List.filter (fun x -> x <> l) basic) in
    let non' = l::(List.filter (fun x -> x <> e) non) in
    (mat,(non',basic'))

  let run_tests times = ()

  (* SOLVE *)
  let rec solve (s: system) : elt option = 
 
    let (mat,(non,basic)) = break_system s in

    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in

    (* checks to see if the column of a given index contains at least one positive value *)
    let check_col (x:int): bool = 
      let (height_col, col) = get_column mat x in
      let rec has_pos (i:int) (arr_x: elt array): bool = 
        if i < height_col then 
          match Elts.compare arr_x.(i) Elts.zero with 
          | Less | Equal -> has_pos (i+1) arr_x
          | Greater -> true
        else false in 
      has_pos 1 col in 

    (* checks to see if the row of a given index contains at least one positive value *)
    let check_row (x:int): bool = 
      let (height_row, row) = get_row mat x in
      let rec has_pos (i:int) (arr_x: elt array): bool = 
        if i < height_row then 
          match Elts.compare arr_x.(i) Elts.zero with 
          | Less | Equal -> has_pos (i+1) arr_x
          | Greater -> true
        else false in 
      has_pos 0 row in 

    (* recursively loops through non to determine entering variable *)
    let rec find_e (non_lst: int list): int option = 
      let (row_length, first_row) = get_row mat 1 in 
        match non_lst with 
        | [] -> None
        | hd::tl -> 
          match Elts.compare first_row.(hd) Elts.zero with 
          | Greater -> 
            if (check_col hd) then (Some hd) 
            else find_e tl 
          | Less | Equal -> find_e tl in
  
   match find_e (List.sort compare non) with 
   | None -> 
     if not(check_row 1) then 
      let solution = get_elt mat (1,p) in
      let _ = Elts.print solution in
      Some solution
     else raise (Failure "unbounded: no solution")
   | Some x -> 
     let s' = pivot s x in 
     solve s'     
 
end
