module type MATRIX =
sig

  exception NonSquare
  exception ImproperDimensions

  type elt

  type matrix

  (* empty matrix *)
  val empty : int -> int -> matrix

  (* Takes a list of lists and converts that to a matrix *)
  val from_list : (elt list list) -> matrix

  (* Scales every element in the matrix by another elt *)
  val scale : matrix -> elt -> matrix

  (* Adds two matrices. They must have the same dimensions *)
  val add : matrix -> matrix -> matrix

  (* Multiplies two matrices. If the matrices have dimensions m x n and p x q, n
   * and p must be equal, and the resulting matrix will have dimension m x q *)
  val mult: matrix -> matrix -> matrix

  (* Returns the row reduced form of a matrix *)
  val row_reduce: matrix -> matrix
  (* We will implement the algorithm found in the link above *)

  (* Returns the inverse of a matrix *) 
  val inverse: matrix -> matrix
    
  (*
  (* Returns the norm of the matrix *)
  val norm: matrix -> elt
  *)

  (*Transposes a matrix. If the input has dimensions m x n, the output will
   * have dimensions n x m *)
  val transpose: matrix -> matrix

  (* Returns the trace of the matrix *)
  val trace: matrix -> elt list

  (* For testing only *)
  val find_max_col_index: elt array -> int option 

  (*
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

  (* Runs tests on the Matrix Module *)
  val run_tests : unit -> unit 

end