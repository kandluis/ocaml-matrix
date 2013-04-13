
(* PROJECT: OCaml Matrix/Min_Mathematica 
 *
 * NAMES:
 *
 * Partner 1's name: Andy Shi
 * Partner 1's github username: shiandy
 *
 * Partner 2's name: Ding Zhou
 * Partner 2's github username: dzhou94
 *
 * Partner 3's name: Jason Wang
 * Partner 3's github username: jasonwang99
 *
 * Partner 4's name: Luis Perez
 * Partner 4's github username: kandluis
 * 
 * Github Organization name: Fantastic-four
 *
 * Github Project URL: github.com/Fantastic-four/ocaml-matrix
 *)

module type MATRIX =
sig
  exception NonSquare
  type elt
  type matrix

  (* Type of this is unknown, but will probably be represented using Ocaml's
   * built-in Arrays *)

  (* empty matrix *)
  val empty

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

  (* Prints out the contents of a matrix *)

  val print :matrix -> unit

  (* Iterate through the matrix and print each element *)
end
