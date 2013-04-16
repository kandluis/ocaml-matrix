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

  (* Transposes a matrix. If the input has dimensions m x n, the output will
   * have dimensions n x m *)

    val transpose: matrix -> matrix

  (* Will basically ``flip'' the indices of the input matrix *)

  (* Returns the trace of the matrix *)

    val trace: matrix -> elt

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
      begin
        (* make a new array to enfore immutability *)
        let row' = Array.make p C.zero in
        for i = 0 to p - 1 do
          row'.(i) <- m.(row - 1).(i)
        done ;
        (p, row')
      end
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
      for i = 0 to p - 1 do
        m.(row).(i) <- a.(i)
      done
    else
      raise (Failure "Row out of bounds.")

  (* Similar to set_row but sets the nth column instead *)
  let set_column (((n,p),m): matrix) (column: int) (a: elt array) : unit =
    if column <= p then
      for i = 0 to n - 1 do
        m.(i).(column) <- a.(i)
      done
    else
      raise (Failure "Columt out of bounds.")

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
      
  let scale (m: matrix) (sc: elt) : matrix = raise TODO


  (* Will implement by iterating through the matrix and scaling each element *)

  let from_list (lsts : elt list list) : matrix = raise TODO 


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
   * and p must be equal, and the resulting matrix will have dimension m x q *)

  let mult (m1: matrix) (m2: matrix) : matrix = raise TODO

  (* Will take the dot product of the nth row of the first matrix and the jth
   * column of the second matrix to create the n,j th entry of the resultant *)

  (* Returns the row reduced form of a matrix *)

  let row_reduce (m1: matrix) : matrix = raise TODO

  (* We will implement the algorithm found in the link above *)

  let print (m: matrix) : unit = raise TODO
end


