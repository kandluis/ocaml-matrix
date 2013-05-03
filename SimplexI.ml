open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig

  type system

  val make_system : matrix -> (int list * int list) -> system

  val break_system : system -> matrix * (int list * int list)

  (*val load : string -> system*)
 
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

  (* 
     helper function to generate int list from a to b, inclusive
     returns empty list if a > b 
  *)
  let rec generate_list (a:int) (b:int) : int list = 
    match a <= b with 
    | true -> a::(generate_list (a+1) b) 
    | false -> [] 

  (* 
     determines whether given matrix is feasible or not, returns None if not
     feasible. If feasible, it will return a system where the basic solution 
     is feasible 
  *)
  let initialize_simplex (mat: matrix) : system option = 
    let elts_neg_one = Elts.substract Elts.zero Elts.one in
    let (m,n) = get_dimensions mat in 
    let b_col = get_column mat n in 

    (* finds the least constant b and returns its index *)
    let rec get_min_b (min_index:int) (i:index): int = 
      if i <= n then 
        match Elts.compare b_col.(i) b_col.(min_index) with 
	| Less -> get_min_b i (i+1)
	| Equal | Greater -> get_min_b min_index (i+1)
      else 
       min_index 
    in 
    let min_index = get_min_b 0 0 in 
    
    (* if the least b is greater than or equal to 0 no modification is 
       needed. If it is less than 0, need to add an additional
       slack variable and pivot so that the solution is 
       feasible *)
    match Elts.compare b_col.(min_index) Elts.zero with 
    | Greater | Equal -> 
      (let new_mat = empty m (m+n-1) in 
       (* copies the coefficients of the constraint functions and objective 
          function into the new matrix *)
       for c = 1 to n-1 do 
         let (_, col) = get_column mat c in
         set_column new_mat c col 
       done;
       
       (* creates an identity matrix for the m-1 introduced slack variables *)
       for r = 2 to m do 
         set_elt new_mat r (n+r-2) Elts.one
       done; 
       
       (* copies the constants b_i's into the last column of the new matrix *)
       let (_, col) = get_column mat n in 
       set_column new_mat (m+n-1) col 
       
       (* returns system *)
       (new_mat, ((generate_list 1 (n-1)), (generate_list (n) (n+m-2))))
      )
    | Less -> 
      (* creates new m by m+n matrix with an additional slack variable. 
         The objective function is now minimizing x_{m+n-1}, the slack variable
      *)
      let new_mat = empty m (m+n) in 
      
      (* copies the coefficients of the constraint functions and objective 
         function into the new matrix *)      
      for c = 1 to n do 
        let (_, col) = get_column mat c in
        set_column new_mat c col 
      done;
      
      (* create an identity matrix for the m-1 slack variables *)
      for r = 2 to m do 
        set_elt new_mat r (n+r-2) Elts.one
      done; 
      
      (* copies the constants b_i's into the last column of the new matrix *)
      let (_, col) = get_column mat n in 
      set_column new_mat (m+n) col 
      
      (* set the m+n-1 column to be -1 *)
      set_column new_mat (m+n-1) (Array.make m (elts_neg_one)); 
      
      (* changing the objective function to be minimizing x_{m+n-1} by setting
         the first column to be (0, 0, 0, ..., -1, 0) *)
      let first_row = Array.make (m+n) (Elts.zero)in 
      first_row.(m+n-1) <- elts_neg_one;  
      set_row new_mat 1 first_row; 
      
      let new_sys = (new_mat, 
                     ((generate_list 1 (n-1)), (generate_list (n) (n+m-1)))) in 
      
      let pivoted_new_sys = pivot new_sys 
      
      
      
    


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

  (* SOLVE *)
  let rec solve (s: system) : elt option = 
 
    let (mat,(non,basic)) = break_system s in

    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in

    (* checks to see if the column of a given index contains at least one 
       positive value *)
    let check_col (x:int): bool = 
      let (height_col, col) = get_column mat x in
      let rec has_pos (i:int) (arr_x: elt array): bool = 
        if i < height_col then 
          match Elts.compare arr_x.(i) Elts.zero with 
          | Less | Equal -> has_pos (i+1) arr_x
          | Greater -> true
        else false in 
      has_pos 1 col in 

    (* checks to see if the row of a given index contains at least one positive 
       value *)
    let check_row (x:int): bool = 
      let (height_row, row) = get_row mat x in
      let rec has_pos (i:int) (arr_x: elt array): bool = 
        if i < height_row then 
          match Elts.compare arr_x.(i) Elts.zero with 
          | Less | Equal -> has_pos (i+1) arr_x
          | Greater -> true
        else false in 
      has_pos 0 row in 

    (* recursively loops through non to determine leaving variable *)
    let rec find_l (non_lst: int list): int option = 
      let (row_length, first_row) = get_row mat 1 in 
        match non_lst with 
        | [] -> None
        | hd::tl -> 
          match Elts.compare first_row.(hd) Elts.zero with 
          | Greater -> 
            if (check_col hd) then (Some hd) 
            else find_l tl 
          | Less | Equal -> find_l tl in
  
   match find_l non with 
   | None -> 
     if not(check_row 1) then (Some (get_elt mat (1,p)))
     else raise (Failure "unbounded: no solution")
   | Some x -> 
     let s' = pivot s x in 
     solve s'     
 
end
