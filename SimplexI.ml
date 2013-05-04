open Order
open Matrix
open Elts
open EltMatrix

module type SIMPLEX =
sig

  (* Special exception to provide more information to the user *)
  exception ImproperInput of string

  (* This is a Simplex system *)
  type system

  (* These next two were exposed for testing *)
  val make_system : matrix -> (int list * int list) -> system

  val break_system : system -> matrix * (int list * int list)

  (* Loads a system from a file whose name is given by string. Returns none if
   * the system can't be solved *)
  val load_file : string -> system option

  val load_data : string -> matrix

  val initialize_simplex : matrix -> system option

  (* val find_one_index : elt array -> int -> int *)

  (* Loads a system from a matrix. Returns none if the system can't be solved *)
  val load_matrix : matrix -> system option
 
  (* Finds the optimum solution to the system *)
  val solve : system -> elt 

  (* Exposed for testing. *)
  val simple_solve : system -> elt * system

  val pivot : system -> int -> int -> system
  
  val run_tests : int -> unit

  val print_system : system -> unit

end 

module Simplex: SIMPLEX =
struct

  exception ImproperInput of string

  (* A system contains a matrix in the canonical tableau form (see simplex
  wikipedia article), plus two int lists which contain the indices of the basic
  and nonbasic variables, respectively *)
  type system = matrix * (int list * int list)

  (* Generates a system from a matrix and two int lists *)
  let make_system (m: matrix) ((lst1, lst2): int list * int list) : system =
   let s = m,(lst1,lst2) in s

  (* Deconstructs a system into its components *)
  let break_system (s: system) : matrix * (int list * int list) =
    let (m,(lst1,lst2)) = s in (m,(lst1,lst2)) 

  (* helper function to generate int list from a to b, inclusive returns empty
   * list if a > b *)
  let rec generate_list (a:int) (b:int) : int list = 
    match a <= b with 
    | true -> a::(generate_list (a+1) b) 
    | false -> [] 
  
  (* helper function to print array for DEBUG *)
  let print_array (arr: elt array) : unit = 
    for i = 0 to (Array.length arr) -1 do
      Elts.print arr.(i); 
      print_string "\n";
    done;
    ()

  (* Prints a system *)
  let print_system (s: system) : unit =
    let m,(n,b) = break_system s in
    print m;
    print_string "\nBasic Variables: ";
    let print_l = List.iter (fun x -> print_string ((string_of_int x) ^ " ")) in
    print_l b;
    print_string "\nNon-Basic Variables: ";
    print_l n;
    ()
    
  (* Helper function. Takes in an array and its length and returns the
   * Matrix (ie non-zero) index of the Elts.one location. Assumes the array
   * contains only one Elts.one *)
  let find_one_index (arr: elt array) (n: int) =
    (* debug *)
    (* print_array arr; *)

    let rec find (i: int) =
      if i < n then
        match Elts.compare arr.(i) Elts.one with
        | Equal -> i+1
        | Greater | Less -> find (i+1)
      else
        raise (Failure "Could not find the constraint!") 
    in (* end of find function *)
    (* SHOULDNT THIS BE FIND 1 INSTEAD OF FIND 0?!?!/1?!*)
    find 1 
 
  (* Pivots a system based on the entering and leaving variables *)
  let pivot (s: system) (e: int) (l: int) : system = 
    let (mat,(non,basic)) = break_system s in
    let (n,p) = get_dimensions mat in
    
    let (_,col) = get_column mat l in
    let row_index = find_one_index col n in
      
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

  (* This solves the simple (ie. solvable) Simplex case. Returns the solution
   * and a system *)
  let rec simple_solve (s: system) : (elt * system) =
    
    (********* "Instance Variables" *************)
    (* debug *)
    (*
    print_string "Starting simple_solve \n";
    let (debug_s', (debug_non, debug_basic)) = break_system s in 
    print debug_s';
    print_string "nonbasic: \n";
    List.iter (print_int) debug_non;
    print_string "\n";
    print_string "basic: \n";
    List.iter print_int debug_basic;
    print_string "\n";
    *)

    let (mat,(non,basic)) = break_system s in
    
    (* We need this to be accessible everywhere *)
    let (n,p) = get_dimensions mat in
    
    (****************** Helper functions *************)

    (* Helper function to see if an array has at least one positive value *)
    let rec has_pos (i:int) (max_i:int) (arr_x: elt array): bool = 
      if i < max_i then 
        match Elts.compare arr_x.(i) Elts.zero with 
        | Less | Equal -> has_pos (i+1) max_i arr_x
        | Greater -> true
      else false 
    in (* end of has_pos *)

    (* Helper function which checks to see if the column of a given index
     * contains at least one positive value *)
    let check_col (x:int): bool = 
      let (height_col, col) = get_column mat x in
      has_pos 1 height_col col 
    in (* end check_col *)

    (* Helper function which checks to see if the row of a given index contains
    at least one positive value *)
    let check_row (x:int): bool = 
      let (height_row, row) = get_row mat x in
      has_pos 0 (height_row - 1) row 
    in (* end check_row *)

    (* Helper function to find the tightest constraint *)
    let min_index (arr_b : elt array) (arr_c : elt array) : int = 
      (* Helper function for min_index *)
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
          min+1 (* matrices are NOT zero indexed *)
      in (* end of index function, continues min_index *)
      match index 1 0 None with 
      | 1 -> raise (Failure "Could not find min_index.")
      | i -> i 
    in (* end of min_index *)

    (* Helper function which recursively loops through nonbasic variables to
     * determine entering variable *)
    let rec find_e (non_lst: int list): int option = 
      let (row_length, first_row) = get_row mat 1 in 
      match non_lst with 
      | [] -> None
      | hd::tl -> 
        match Elts.compare first_row.(hd-1) Elts.zero with 
        | Greater -> 
          if (check_col hd) then (Some hd) 
          else find_e tl 
        | Less | Equal -> find_e tl 
    in (* end find_e *)
    
    (* debug *)
    (*
    print_string "debuggin find_e, nonbasic variables are:  \n";
    List.iter print_int non;
    if find_e (List.sort compare non) = None then print_string "Returns None\n" 
    else print_string "Doesn't return None\n";
    *)

    (* Helper function which finds the leaving variable in a given row *)
    let rec find_leaving (lst: int list) (row_index: int) : int option =
      match lst with
      | [] -> None
      | hd::tl -> 
        let elt = get_elt mat (row_index,hd) in
        match Elts.compare elt Elts.one with
        | Equal -> Some hd
        | Less | Greater -> find_leaving tl row_index
    in (* end of find_leaving *)

    (************** Main simple_solve code ************)
    match find_e (List.sort compare non) with 
    | None -> 
      (if not(check_row 1) then 
        begin
          let solution = get_elt mat (1,p) in

          (* debug *)
          (*
          print_string "THIS IS DONE!!!!!!!\n";
          Elts.print solution;
          print_string "\n";
          *)

          (solution,s)
        end 
      else raise (Failure "unbounded: no solution"))
    | Some e -> 
      (
      (* gets our entering column *)
      let (len1,column) = get_column mat e in
      
      (* gets our constants column *)
      let (len2,last) = get_column mat p in  
      let _ = assert(n = len1) in
      let _ = assert(n = len2) in

      (* finds the row with the maximum constraint *)
      let row_index = min_index column last in

      let l =
        match find_leaving basic row_index with
        | None -> raise (Failure "Could not find entering variable")
        | Some x -> x in
      
      (* debug print out entering leaving variable *)
      (*
      print_string "entering, leaving:";
      print_int e;
      print_int l;
      print_string "\n";
      *)

      let s' = pivot s e l in 
      
      (* debug *)
      (*
      let (debug_s', (debug_non, debug_basic)) = break_system s' in 
      print debug_s';
      print_string "nonbasic: \n";
      List.iter (print_int) debug_non;
      print_string "\n";
      print_string "basic: \n";
      List.iter print_int debug_basic;
      print_string "\n";
      *)
      
      simple_solve s'  
      )
      (* end of Some case *)

  (* end of simple_solve *)

  (* determines whether given matrix is feasible or not, returns None if not
   * feasible. If feasible, it will return a system where the basic solution is
   * feasible *)
  let initialize_simplex (mat: matrix) : system option = 

    (************ "Instance variables" **********)
    let elts_neg_one = Elts.subtract Elts.zero Elts.one in
    let (m,n) = get_dimensions mat in 
    let (_,b_col) = get_column mat n in 

    (************ Helper functions **************)
    (* finds the least constant b and returns its index *)
    let rec get_min_b (min_index:int) (i:int): int = 
      if i < m then 
        (* Arrays are 0-indexed *)
        match Elts.compare b_col.(i) b_col.(min_index) with 
         | Less -> get_min_b i (i+1)
         | Equal | Greater -> get_min_b min_index (i+1)
      else 
       min_index
    in (* end get_min_b *)

    (* Helper to find entering variable in a given row of a matrix *)
    let rec find_entering (mat: matrix) (row: int) (lst: int list) : int =
      match lst with
      | [] -> raise (Failure "Could not find entering")
      | hd::tl ->
        let constant = get_elt mat (row, hd) in
        match Elts.compare constant Elts.zero with
        | Equal -> find_entering mat row tl
        | Greater | Less -> hd 
    in (* end find_entering *)

    (* Helper function needs commenting *)
    let rec filter_and_decrease (lst: int list) (max_dim: int) : int list =
      match lst with
      | [] -> []
      | hd::tl ->
        if hd = (max_dim - 1) then filter_and_decrease tl max_dim
        else if hd > (max_dim - 1) then (hd-1)::filter_and_decrease tl max_dim
        else (* hd < dimy - 1 *) hd::filter_and_decrease tl max_dim
    in (* end of filter_and_decrease *)

    (* Helper function needs commenting *)
    let skip_find_one_index (arr: elt array) (start: int) : int =
      let rec helper (i: int) : int =
        if i < m then
          match Elts.compare arr.(i) Elts.one with
          | Greater | Less -> helper (i + 1)
          | Equal -> i + 1 
        else 
          raise (Failure "The column did not have a one!?") 
      in
      helper (start - 1) 
    in (* end skip_find_one_index *)

    (* Helper function needs commenting *)
    let rec substitute (mat: matrix) (lst: int list) : unit =
      match lst with
      | [] -> ()
      | hd::tl ->
        let (_,col) = get_column mat hd in
        let row_index = skip_find_one_index col 2 in
        sub_mult mat 1 row_index (get_elt mat (1, hd));
        substitute mat tl 
    in (* end substitute *)

    (************ Main initialize_simplex code ***********)

    let min_index = get_min_b 1 1 in 

    (* debug *)
    (* print_int min_index; *)

    (* if the least b is greater than or equal to 0 no modification is 
     * needed. If it is less than 0, need to add an additional
     * slack variable and pivot so that the solution is 
     * feasible *)
    match Elts.compare b_col.(min_index) Elts.zero with 
    | Greater | Equal -> 
      ( 
      (* debug *)
      (* print_string "It's greater orequal !!!"; *)

      let dimx,dimy = m, (m+n-1) in
      let new_mat = empty dimx dimy in 
      (* copies the coefficients of the constraint functions and objective 
        function into the new matrix *)
      for c = 1 to n-1 do 
        let (_, col) = get_column mat c in
        set_column new_mat c col 
      done;
      
      (* creates an identity matrix for the m-1 introduced slack variables *)
      for r = 2 to m do 
        set_elt new_mat (r,(n+r-2)) Elts.one
      done; 
      
      (* copies the constants b_i's into the last column of the new matrix *)
      let (_, col) = get_column mat n in 
      set_column new_mat dimy col; 
      
      (* debug *)
      (* print new_mat; *)

      (* returns system *)
      Some (new_mat, ((generate_list 1 (n-1)), (generate_list (n) (n+m-2))))
      ) (* end of Greater | Equal case *)

    | Less -> 
      (
      (* Creates new m by m+n matrix with an additional slack variable. 
       * The objective function is now minimizing x_{m+n-1}, the slack variable
       *)
      (* debug *)
      (* print_string "Less!!!"; *)

      let dimx, dimy = m, m+n in
      let new_mat = empty dimx dimy in 
      
      (* copies the coefficients of the constraint functions and objective 
       * function into the new matrix *)      
      for c = 1 to n -1 do 
        let (_, col) = get_column mat c in
        set_column new_mat c col 
      done;
      
      (* create an identity matrix for the m-1 slack variables *)
      for r = 2 to m do 
        set_elt new_mat (r, (n+r-2)) Elts.one
      done; 
      
      (* copies the constants b_i's into the last column of the new matrix *)
      let (_, col) = get_column mat n in 
      set_column new_mat dimy col; 
      
      (* set the m+n-1 column to be -1 *)
      set_column new_mat (dimy-1) (Array.make m (elts_neg_one)); 

      (* Set the top row as the new constraint *)
      let constraint' = Array.make dimy Elts.zero in
      constraint'.(dimy-2) <- elts_neg_one;
      set_row new_mat 1 constraint';
      
      (* Making our new system! This is the original EXCEPT for the objective *)
      let new_sys = (new_mat, 
                     ((dimy-1)::(generate_list 1 (n-1)), (generate_list (n) 
                                                         (dimy-2)))) in
      
      (* We pivot once to return a solvable system *)
      let pivoted_new_sys = pivot new_sys (dimy-1) (min_index+n-1) in

      (* debug to print out debug_mat *)
      (*
      let (debug_mat, _) = break_system pivoted_new_sys in 
      print debug_mat;
      *)

      (* We solve the system, returning the value and the new system *)
      let elt_answer, s' = simple_solve pivoted_new_sys in
      print_system s';

      (* debug *)
      (* print_string "The answer should be this: "; *)

      (* Breaking our returned system because we need access to non and basic *)
      let (m',(non',basic')) = break_system s' in

      (* If the solution to our pivoted system is not zero, then our original 
       * system is unfeasable, so return None *)
      match Elts.compare Elts.zero elt_answer with
      | Greater | Less -> None
      | Equal -> 
        (
        let correct_system = 
          (* Check to see if our added slack variable is a non-basic variable *)
          if List.mem (dimy-1) basic' then
            let _ = print_string "If statement reached!\n\n\n" in
            let (len,col) = get_column m' (dimy-1) in
            let row_index = find_one_index col len in
            let entering = find_entering m' row_index non' in
            pivot s' entering (dimy-1)
          else 
            s' in

          let (mat',(non_fin,basic_fin)) = break_system correct_system in
          let final_matrix = empty m (dimy-1) in
          for c = 1 to n+m-1 do
            if c < dimy-1 then
              let (_,col) = get_column mat' c in
              set_column final_matrix c col
            else 
              let (_,col) = get_column mat' (c+1) in
              set_column final_matrix c col
          done;
          let (_,old_obj) = get_row mat 1 in
          let slacked_obj = Array.make (dimy-1) Elts.zero in
          for c = 1 to n-1 do
              slacked_obj.(c-1) <- old_obj.(c-1)
          done;
          slacked_obj.(dimy-2) <- old_obj.(n-1); 
          set_row final_matrix 1 slacked_obj;

          (* Since we removed a slack, we need to decrease our basic list *)
          let basic_fin = filter_and_decrease basic_fin dimy in 


          let _ = substitute final_matrix basic_fin in
            Some (final_matrix,(non_fin,basic_fin))
          ) (* End of Equal match case *)
      ) (* End of Less match case *)
  (* End initialize_simplex *)
(*
              raise (Failure "The column did not have a one!?") in
          helper (start - 1) in
        let rec substitute (lst: int list) : unit =
          match lst with
          | [] -> ()
          | hd::tl ->
            let (_,col) = get_column final_matrix hd in
            let skip_row_index = skip_find_one_index col 2 in
            sub_mult final_matrix 1 skip_row_index 
                      (get_elt final_matrix (1, hd));
            substitute tl in
        let _ = substitute basic_fin in
          Some (final_matrix,(non_fin,basic_fin))
        (* End of Less match case *)

 (* End initialize_simplex *)
>>>>>>> 201c1caa9cbebebfc2eb1e7518103ba58a775999
*)
 
  (* Actually solves a system *)
  let solve (s: system) : elt =
    let (elt,_) = simple_solve s in elt

  
  (************ Functions for I/O *************)

  (************ Helper functions *************)

  (* Some nice wrapping to provide more descriptive error handling *)
  (* Reads an Elt from a string, raises ImproperInput if it fails *)
  let read_elt (s: string) : elt =
    try 
      Elts.from_string s 
    with
    | Elts.NonElt ->
      raise (ImproperInput "You must input valid constraint Elts!")

  (* Assumes that the channel passed in is at the point of loading the objective
   * and attempts to load the following line as an objective function int an 
   * elt list. Raises ImproperInput if it encounters a non-elt value *)
  let load_objective (chan: in_channel) : elt list = 
    let line = input_line chan in
    let line_list = Helpers.explode line "," in
    List.map read_elt line_list
  
  (* Loads one constraint from the exploded string list of elements
   * Returns a string * elt list. Returns the constraint and the elt list *)  
  let rec load_constraint_helper (lst: string list) : 
                                 (string option * elt list) =
    match lst with
    | [] -> (None,[])
    | hd::tl ->
      match load_constraint_helper tl with
      | (None,elts) -> 
        (match hd with 
        | "<=" | ">=" | "=" -> (Some hd, elts)
        | s -> (None, (read_elt s)::elts))
      | (Some v, elts) -> 
        match hd with
        | "<=" | ">=" | "=" -> raise (ImproperInput "Constraint mismatch!")
        | s -> (Some v, (read_elt s)::elts)

  (* Assumes that the channel passed in is at the point of loading the 
   * constraints. Reads one line at a time, adding it to an elt list list
   * until the end of the file is reached. *)
  let load_constraints (chan: in_channel) : elt list list =
    let rec load (built: elt list list) : elt list list =
      try
        let line = input_line chan in
        let line_list = Helpers.explode line "," in
        match load_constraint_helper line_list with
        | None, _ -> raise (ImproperInput "Constraint mismatch!")
        | Some s, elt_lst ->
          let neg_one = Elts.subtract Elts.zero Elts.one in
          if s = "<=" then load (elt_lst::built)
          else if s = ">=" then 
            load ((List.map (Elts.multiply neg_one) elt_lst)::built)
          else if s = "=" then 
            let neg_lst = List.map (Elts.multiply neg_one) elt_lst in
            load (elt_lst::neg_lst::built)
          else
            raise (Failure "Code went awry!")
        with 
        | End_of_file -> built in
    let line = String.lowercase (input_line chan) in
    match line with
    | "subject to" | "subject to\r" -> load []
    | _ -> raise (ImproperInput line)
    

  (* Applies f to every element of the list except the last one *)
  let rec special_map (f: 'a -> 'a) (lst: 'a list) : 'a list =
    match lst with
    | [] -> []
    | hd::[] -> hd::[]
    | hd::tl -> (f hd)::(special_map f tl)

  (* Reads the data from an input stream and returns a list list 
   * with the right values for Simplex. ie, it flips signs if maximizing
   * objective *)
  let read_data (chan: in_channel) : (elt list list) =
    let line = String.lowercase (input_line chan) in
    match line with
    | "min" | "min\r" -> 
      (* Our algorithm minimizes by default, so this is moving all the 
       * constraints to the other side *)
      let neg_one = Elts.subtract Elts.zero Elts.one in
      let obj_lst = special_map (Elts.multiply neg_one) (load_objective chan) in
      let cons_lsts = load_constraints chan in
      obj_lst::cons_lsts
    | "max" | "max\r" ->
      let obj_lst = load_objective chan in
      let cons_lsts = load_constraints chan in
      obj_lst::cons_lsts 
    | _ -> raise (ImproperInput "Missing \"min\" or \"max\" on line 1") 

  (* Loads a matrix from a file given by file *)
  let load_data (file: string) : matrix =
    try
      let chan = open_in file in
      let input_list = read_data chan in
      from_list input_list
    with
      | Sys_error e -> raise(Failure e)

  (*********** Main I/o Functions ***********)

  (* Tries to load a solvable system from a file *)
  let load_file (file: string) : system option =
    initialize_simplex (load_data file)

  (* Load a system given a matrix. Returns non if the system is
   * no solvable *)
  let load_matrix (m: matrix) : system option =
    initialize_simplex m 

  (* Load a system in matrix format from a file. Returns none is 
   * not solvable *)
  let load_matrix_file (s: string) : system option =
    load_matrix (load s)

  let run_tests times = ()
 
end
