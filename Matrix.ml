open Order
open Elts

exception TODO

module type MATRIX =
sig

  exception NonSquare
  exception ImproperDimensions

  type elt

  type matrix

  (* Type of this is unknown, but will probably be represented using Ocaml's
   * built-in Arrays *)

  (* empty matrix *)
  val empty : int -> int -> matrix

  (* Takes a list of lists and converts that to a matrix *)
  val from_list : (elt list list) -> matrix

  (* Will implement using nested match statements *)

  (* Scales every element in the matrix by another elt *)
  val scale : matrix -> elt -> matrix

  (* Will implement by iterating through the matrix and scaling each element *)

  (* Adds two matrices. They must have the same dimensions *)
  val add : matrix -> matrix -> matrix

  (* Will add the elements elementwise and construct a new matrix *)

  (* Multiplies two matrices. If the matrices have dimensions m x n and p x q, n
   * and p must be equal, and the resulting matrix will have dimension m x q *)
  val mult: matrix -> matrix -> matrix

  (* Will take the dot product of the nth row of the first matrix and the jth
   * column of the second matrix to create the n,j th entry of the resultant *)

  (* For testing only *)
  val find_max_col_index: elt array -> int option 

  (* Returns the row reduced form of a matrix *)
  val row_reduce: matrix -> matrix
  (* We will implement the algorithm found in the link above *)

  (* This part will not be used since it is not essential to the simplex
   * algorithm. We will implement it if we have time. *)
  (*
  (* Returns the inverse of a matrix *)
    
  val inverse: matrix -> matrix
    
  (* Will implement this based on the specification in the Algorithms book *)

  (* Returns the norm of the matrix *)

    val norm: matrix -> elt
  *)
  (*Transposes a matrix. If the input has dimensions m x n, the output will
   * have dimensions n x m *)

  val transpose: matrix -> matrix


  (* Will basically ``flip'' the indices of the input matrix *)

  (* Returns the trace of the matrix *)

  val trace: matrix -> elt list

  (*
  (* Will check if the matrix is square, then sum up all the elements along its
   * diagonal *)

  (* Returns the determinant of the matrix *)
  
  val det: matrix -> elt

  (* Will implement this algorithm based on a description in Hubbard. Involves
   * column reducing the input (or row-reducing the transpose) and then keeping
   * track of the operations to build a sequence of coefficients to multiply *)

  (* Returns a list of eigenvalues and eigenvectors of a matrix *)

  val eigen: matrix -> (elt *matrix) list option

  (* Calculates successive powers of the input matrix, each multiplied by the
   * same basis vector. Generates a polynomial and solves for zeros, which
   * yields eigenvalues. Repeat for all basis vectors *)

  (* Takes a string and builds a matrix from it *)
    
  val from_string : string -> matrix
    
  (* We will have some way to express matrices using strings, and then we will
    * parse the string to give the matrix *)
  *)
    
  (* Prints out the contents of a matrix *)
  val print : matrix -> unit

  (* Iterate through the matrix and print each element *)
end

module Matrix (C: ORDERED_AND_OPERATIONAL) : (MATRIX with type elt = C.t) =
struct

  exception NonSquare
  exception ImproperDimensions
  
  type elt = C.t
  
  (* A matrix is a pair of dimension (n x p) and a array of arrays
   * the first array is the row (n) and the second the column (p) *)
  type matrix = (int * int) * (elt array array) 

  (* Type of this is unknown, but will probably be represented using Ocaml's
   * built-in Arrays *)

  (* catching negative dimensions AND 0 dimensions and too large 
   * of a dimension so we don't have to worry about it later *)
  let empty (rows: int) (columns: int) : matrix = 
    if rows > 0 && columns > 0 then
      try
        let m = Array.make_matrix rows columns C.zero in ((rows,columns),m)
      with Invalid_argument "index out of bounds" ->
        raise ImproperDimensions
    else (* dimension is negative or 0 *)
      raise ImproperDimensions

  (** Helper Functions Section **)

  (* get's the nth row of a matrix and returns (r, row) where r is the length
   * of the row and row is a COPY of the original row. For example, calling
   * calling get_row m 1 will return (3, |1 3 4 |) 
   *         ________
   *    m = | 1 3 4 | 
   *        |*2 5 6 | 
   *)
  (* aside: we don't check whether n < 1 because of our matrix invariant *)
  let get_row (((n,p),m): matrix) (row: int) : int * elt array =
    if row <= n then 
      let row' = Array.map (fun x -> x) m.(row - 1) in
      (p, row')
    else 
      raise (Failure "Row out of bounds.")

  
  (* similar to get_row. For m, get_column m 1 will return (2, |1 2|) *)
  let get_column (((n,p),m): matrix) (column: int) : int * elt array =
    if column <= p then 
      begin
        let column' = Array.make n C.zero in
        for i = 0 to n - 1 do
          column'.(i) <- m.(i).(column - 1)
        done;
        (n, column')
      end
    else
      raise (Failure "Column out of bounds.")

  (* sets the nth row of the matrix m to the specified array a. 
   * This is done IN-PLACE. Therefore the function returns unit.
   * You should nonetheless enfore immutability whenever possible. 
   * For a clarification on what nth row means, look at comment for
   * get_row above. 
   *)
  let set_row (((n,p),m): matrix) (row: int) (a: elt array) : unit =
    if row <= n then 
      for i = 0 to n - 1 do
        m.(row - 1).(i) <- a.(i)
      done
    else
      raise (Failure "Row out of bounds.")

  (* Similar to set_row but sets the nth column instead *)
  let set_column (((n,p),m): matrix) (column: int) (a: elt array) : unit =
    if column <= p then
      for i = 0 to n - 1 do
        m.(i).(column - 1) <- a.(i)
      done
    else
      raise (Failure "Column out of bounds.")

  (* returns the ij-th element of a matrix (not-zero indexed) *)
  let get_elt (((n,p),m): matrix) ((i,j): int*int) : elt =
    if i <= n && p <= j then
      m.(i - 1).(j - 1)
    else 
      raise ImproperDimensions

  (* similar to map, but applies to function to the entire matrix 
   * Returns a new matrix *)
  let map (f: 'a -> 'b) ((dim,m): matrix) : matrix = 
    (dim, Array.map (Array.map f) m)

  (* Just some wrapping of Array.iter made for Matrices! *)
  let iter (f: 'a -> unit) ((dim,m): matrix) : unit =
    Array.iter (Array.iter f) m

  (* Just some wrapping of Array.iteri. Useful for pretty
   * printing matrix. The index is (i,j). NOT zero-indexed *)
  let iteri (f: int -> int -> 'a -> unit) ((dim,m): matrix) : unit =
    Array.iteri (fun i row -> Array.iteri (fun j e -> f (i+1) (j+1) e) row) m 

  (* folds over each row using base case u and function f, 
   * creating a new array with the results. Then it folds over that
   * array with function g and base case v, returning a v.
   *)
  (* could be a bit more efficient? *)
  let reduce (f: 'a -> 'b -> 'a) (u: 'a) (((p,q),m): matrix) : 'b =
    let total = ref u in
      for i = 0 to p - 1 do
        for j = 0 to q - 1 do
          total := f (!total) m.(i).(j) 
        done;
      done;
    !total

  (* given two arrays, this will calculate their dot product *)
  (* It seems a little funky, but this is done for efficiency's sake.
   * In short, it tries to multiply each element by it's respective
   * element until the one array is indexed out of bounds. If the
   * other array is also out of bounds, then it returns their value.
   * Otherwise, the arrays were the wrong size and raises ImproperDimension

    THE ABOVE COMMENT HAS NOT BEEN IMPLEMENTED 
   *)
  let dot (v1: elt array) (v2: elt array) : elt =
    let rec dotting (i: int) (total: elt) : elt =
      if i = 0 then total
      else 
        let curr = C.multiply v1.(i-1) v2.(i-1) in
        dotting (i - 1) (C.add curr total) in
    let len1, len2 = Array.length v1, Array.length v2 in
    if len1 = len2 then dotting len1 C.zero
    else raise ImproperDimensions
    
  (** End Helper Functions **)
      
  let scale (m: matrix) (sc: elt) : matrix = map (C.multiply sc) m

  (* Will implement by iterating through the matrix and scaling each element *)

  (* This takes in a list of lists. The inners lists are the rows *)
  let from_list (lsts : elt list list) : matrix = 
    let rec check_length (length: int) (lst: elt list) : int =
      if List.length lst = length then length
      else raise (Failure "Rows are not all the same length!") in 
    let p = List.length lsts in
    match lsts with
    | [] -> empty 1 1
    | hd::tl -> 
      let len = List.length hd in
      if List.fold_left check_length len tl = len then
        ((p,len),Array.map Array.of_list (Array.of_list lsts))
      else
        raise (Failure "Rows are not all the same length")



  (* Adds two matrices. They must have the same dimensions *)

  let add ((dim1,m1): matrix) ((dim2,m2): matrix) : matrix =
    if dim1 = dim2 then
      let n, p = dim1 in
      let (dim', sum_m) = empty n p in
      for i = 0 to n - 1 do
        for j = 0 to p - 1 do
          sum_m.(i).(j) <- C.add m1.(i).(j) m2.(i).(j)
        done;
      done;
      (dim',sum_m)
    else
      raise ImproperDimensions
    

  (* Will add the elements elementwise and construct a new matrix *)

  (* Multiplies two matrices. If the matrices have dimensions m x n and p x q, n
   * and p must be equal, and the resulting matrix will have dimension n x q *)

  let mult (matrix1: matrix) (matrix2: matrix) : matrix =  
    let ((m,n),m1), ((p,q),m2) = matrix1, matrix2 in
    if n = p then
      let (dim, result) = empty m q in
      for i = 0 to m - 1 do
        for j = 0 to q - 1 do
          let (_,row), (_,column) = get_row matrix1 (i + 1), get_column matrix2 (j + 1) in
          result.(i).(j) <- dot row column
        done;
      done;
      (dim,result)
    else
      raise ImproperDimensions
          
  (* Will take the dot product of the nth row of the first matrix and the jth
   * column of the second matrix to create the n,j th entry of the resultant *)

  (* Returns the row reduced form of a matrix *)
  (** Helper functions for row_reduce **)

  (* returns the index of the first non-zero elt in an array*)
  let zero (arr: elt array) : int option =
    let index = ref 1 in
    let empty (i: int option) (e: elt) : int option =
      match i, C.compare e C.zero with
      | None, Equal -> (index := !index + 1; None)
      | None, _ -> Some (!index) 
      | _, _ -> i in
    Array.fold_left empty None arr

  (* returns the the location of the nth non-zero
   * element in the matrix. Scans column wise.
   * So the nth non-zero element is the FIRST non-zero
   * element in the nth non-zero column *)
  let nth_nz_location (m: matrix) (n: int): (int*int) option =
    let ((n,p),m') = m in
    let rec check_col (to_skip: int) (j: int) =
      if j <= p then
        let (_,col) = get_column m j in
        match zero col with
        | None -> check_col to_skip (j + 1)
        | Some i -> 
          if to_skip = 0 then 
            Some (i,j)
          else (* we want a later column *) 
            check_col (to_skip - 1) (j + 1)
      else None in
    check_col (n - 1) 1

  (* returns the the location of the first
   * non-zero and non-one elt. Scans column wise, from
   * left to right. Basically, it ignores columns
   * that are all zero or that *)
  let fst_nz_no_loc (m: matrix): (int*int) option =
    let ((n,p),m') = m in
    let rec check_col (j: int) =
      if j <= p then
        let (_,col) = get_column m j in
        match zero col with
        | None -> check_col (j + 1)
        | Some i -> 
          match C.compare col.(i-1) C.one with
          | Equal -> check_col (j + 1)
          | _ -> Some (i,j)
      else None in
    check_col 1

  (* Finds the element with the greatest absolute value in a column. Is not 
   * 0-indexed. If two elements are both the maximum value, returns the one with
   * the lowest index. Returns None if this element is zero (if column is all 0)
   *)

  let find_max_col_index (array1: elt array) : int option = 
    (* Compares two elements in an elt array and returns the greater and its
     * index *)
    let compare_helper (e1: elt) (e2: elt) (ind1: int) (ind2: int) : (elt*int) = 
      match C.compare e1 e2 with 
      | Equal -> (e2, ind2)
      | Greater -> (e1, ind1)
      | Less -> (e2, ind2) 
    in

    (* Helper function *) 
    let rec find_index (max_index: int) (curr_max: elt) (curr_index: int) 
        (arr: elt array) = 
      if curr_index = Array.length arr then 
        (if curr_max = C.zero then None
        else Some (max_index+1)) (* Arrays are 0-indexed but matrices aren't *)
      else
        (match C.compare arr.(curr_index) C.zero with
        | Equal -> find_index max_index curr_max (curr_index+1) arr
        | Greater -> 
          (let (el, index) = compare_helper (arr.(curr_index)) curr_max curr_index
            max_index in
          find_index index el (curr_index+1) arr)
        | Less -> 
          (let abs_curr_elt = C.subtract C.zero arr.(curr_index) in
          let (el, index) = compare_helper abs_curr_elt curr_max curr_index
            max_index in
          find_index index el (curr_index+1) arr))
    in
    find_index 0 C.zero 0 array1

  (* Basic row operations *)
  let scale_row (m: matrix) (num: int) (sc: elt) : unit = 
    let (len, row) = get_row m num in 
    let new_row = Array.map (fun a -> C.multiply sc a) row in
    set_row m num new_row

  let swap_row (m: matrix) (r1: int) (r2: int) : unit =
    let (len1, row1) = get_row m r1 in
    let (len2, row2) = get_row m r2 in
    let _ = assert (len1 = len2) in 
    let _ = set_row m r1 row2 in 
    let _ = set_row m r2 row1 in
    ()

  (* Subtracts a multiple of r2 from r1 *)
  let sub_mult (m: matrix) (r1: int) (r2: int) (sc: elt) : unit = 
    let (len1, row1) = get_row m r1 in
    let (len2, row2) = get_row m r2 in
    let _ = assert (len1 = len2) in
    for i = 0 to len1 - 1 do (* Arrays are 0-indexed *)
      row1.(i) <- C.subtract row1.(i) (C.multiply sc row2.(i))
    done;;

  let row_reduce (mat: matrix) : matrix =
    let rec row_reduce_h (n_row: int) (n_col: int) (mat2: matrix) : unit = 
      let ((num_row, num_col), arr) = mat2 in
      if (num_row < n_row) && (num_col < n_col) then ()
      else
        (let (_,col) = get_column mat2 n_col in
        match find_max_col_index col with
        | None (* Column all 0s *) -> row_reduce_h (n_row+1) (n_col+1) mat2 
        | Some index -> 
          (* if index <> n_row then swap_row mat2 index n_row; *)
          (swap_row mat2 index n_row; 
          let pivot = get_elt mat2 (n_row, n_col) in
          scale_row mat2 (n_row+1) (C.divide C.one pivot);
          for i = 1 to num_row do
            if i <> n_row then sub_mult mat2 i n_row (get_elt mat2 (i,n_col))
          done;
          row_reduce_h (n_row+1) (n_col+1) mat2))
    in
    let ((n,p),m) = mat in
    let (dim,mat_cp) = empty n p in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        mat_cp.(i).(j) <- m.(i).(j)
      done;
    done;
    let _ = row_reduce_h 1 1 (dim,mat_cp) in (dim,mat_cp)

  let print (m: matrix) : unit =
    let ((row,col), m') = m in
    let pretty_print (_: int) (j: int) (e: elt) =
      if j = 1 then
        print_string "|"
      else ();
      C.print e;
      print_char ' ';
      if j = col then
        print_string "|\n"
      else () in
    iteri pretty_print m


  (** Optional module functions **)

  (* calculates the trace of a matrix and returns it as an elt list *)
  let trace (((n,p),m): matrix) : elt list =
    let rec build (lst: elt list) (i: int) =
      if i > 0 then
        build (m.(i).(i)::lst) (i - 1)
      else
        lst in
    if n = p then build [] n
    else raise ImproperDimensions 

  (* calculates the transpose of a matrix and retuns a new one *)
  let transpose (((n,p),m): matrix) =
    let (dim,m') = empty p n in
    for i = 0 to n - 1 do
      for j = 0 to p - 1 do
        m'.(j).(i) <- m.(i).(j)
      done;
    done;
    assert(dim = (p,n));
    ((p,n),m')
end

module FloatMatrix = Matrix(Floats) ;;
let a = Floats.generate ();;
let b = Floats.generate_gt a ();;
let c = Floats.generate_gt b ();;
let d = Floats.generate_gt c ();;
let test1 = FloatMatrix.from_list [[a;b];[c;d]];;
FloatMatrix.print test1;;

let test2 = FloatMatrix.from_list[[a;a];[c;c]];;
let test12 = FloatMatrix.add test1 test2;;
FloatMatrix.print test12;;
let test3 = Array.make 2 a;;
match FloatMatrix.find_max_col_index test3 with
| None -> print_string ("None")
| Some index -> print_string (string_of_int index)
;;

let test1_reduced = FloatMatrix.row_reduce test1;;
FloatMatrix.print test1_reduced;;

