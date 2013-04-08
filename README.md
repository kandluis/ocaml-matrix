ocaml-matrix
============

Matrix Module and Arithmetic system for Ocaml.CS 51 Final Project Draft Specication
Luis Perez j luisperez@college.harvard.edu
Andy Shi j andyshi@college.harvard.edu
Zihao Wang j zihaowang01@college.harvard.edu
Ding Zhou j dzhou@college.harvard.edu
April 7, 2013
1 Brief Overview
Our project seeks to implement an ecient matrix library for Ocaml. The end goal is to utilize
this matrix library in conjunction with the simplex algorithm to nd solution to problems in linear
programming. In order to accomplish this, there will be multiple small checkpoints. We need to
implement and ecient matrix representation (so as to not use the naive implementation of lists
of lists). We also hope to provide a useful interface that allows the user to input arbitrarily large
matrices eectively (possible by mapping over the entries of the matrix). One big challenge for
the matrix library will be the implementation of ecient algorithms for multiplication and row
reduction. We expect the multiplication to initially consists of the standard algorithm, but will
eventually grow into either block multiplication or Strassen. We might even implement some way
to chose between the algorithms depending on the inputted matrices, but that would not an essential
feature.
The row reduction algorithm will certainly be one of the most fundamental algorithms in our matrix
library implementation. From our current understanding of simplex, it seems like it will heavily
rely on row reduction. The only algorithm we can come up with so far is Gauss-Jordan, though we
have found some algorithms which are more suited for computation by computers with imprecise
storage of 
oats.
The simplex part of the problem is the main goal, but if we can get down the matrix module, then it
appears as if the simplex algorithm should be straightforward. Eventually, we wish to implement a
simplex module that allows the input of arbitrary linear equations with linear constraints, and then
we parse this information into the corresponding matrix and nally perform the simplex algorithm
on that matrix in order to return a solution to the specied problem.
Other tentative goals are to implement matrices that can accept either arbitrarily large numbers
(bignums) or arbitrarily precise 
oats (int list * int stream). It would also be interesting,
though not necessary, to implement multiple algorithms for each operation and choose from them
the one that is the best for a specic input data.
1
CS51 Draft Spec April 7, 2013
2 Feature List
1. Find a way to represent a matrix (arrays or list list or other representation)|will likely use
an array representation using the Ocaml array module (http://caml.inria.fr/pub/docs/
manual-ocaml/libref/Array.html)
2. Make a matrix from a list of lists (this is how the user will input the data of the matrix).
3. Scalar multiplication|as described here (http://www.purplemath.com/modules/mtrxmult.
htm)
4. Matrix addition|as described here (http://www.purplemath.com/modules/mtrxmult.htm)
5. Standard Matrix multiplication (http://www.purplemath.com/modules/mtrxmult.htm)
6. Row Reduction (Gauss-Jordan). We will use the following resource to help us calculate
row reduction when the entries of the matrix are stored with limited precision (http://
thejuniverse.org/PUBLIC/LinearAlgebra/LOLA/rowRed/var.html)
7. Matrix inverse / Row reduction with LU Decomposition - Description can be found here
(http://www.math.ust.hk/
~
macheng/math111/LU_Decomposition.pdf)
8. Simplex Algorithm (Described in Algorithms book, in addition to the resources provided
below)
9. Matrix norm
10. Matrix transpose
11. Trace|This is a typical trace (sum of the entries on the diagonal)
12. Determinant|found using algorithm described in Section 4.8 of Hubbard's Vector Calculus,
Linear Algebra, and Dierential Forms: A Unied Approach (Math 23a/b textbook)
13. Eigenvectors / Eigenvalues, calculated also by a method in Hubbard's book (this might not
be necessary to carry out the simplex algorithm and we might abandon it if we can't gure
out how to implement it in a timely manner)
3 Draft Technical Specication
3.1 Matrices
Our matrix module will actually be a functor that takes in an ORDERED_AND_OPERATIONAL module
(from ps4 and moogle) so we can have matrices of ints, 
oats, or 
oats as streams.
The following type denition (a la ps4) will give us a way to talk about order for dierent data
types.
type order = Equal | Less | Greater
2
CS51 Draft Spec April 7, 2013
The following signature will allow us to provide matrix functionality on a variety of data types. For
example, we could make an ORDERED_AND_OPERATIONAL type for 
oats, where add would be dened
as (+.), for example. In our matrix functor, we will then use the functions dened in the signature
below (for example, add instead of (+.)). Also, we have included some generating functions to help
us with testing.
module type ORDERED_AND_OPERATIONAL
sig
type t
val zero : t
val one: t
val compare : t -> t -> order
(* Converts a t to a string *)
val to_string : t -> string
val add: t -> t -> t
val subtract: t -> t -> t
val multiply: t -> t -> t
val divide: t -> t -> t
(* For testing *)
(* Prints a t *)
val print: t -> unit
(* Generates the same t each time when called *)
val generate: unit -> t
(* Generates a t greater than the argument passed in *)
val generate_gt: t -> unit -> t
(* Generates a t less than the argument passed in *)
val generate_lt: t -> unit -> t
(* Generates a t in between the two arguments. Returns none if none exist *)
val generate_between: t -> t -> unit -> t option
end
The following signature will provide all the necessary functions that can be performed on a matrix.
We include basic matrix operations like additions, multiplications, scalar multiplication, and then
standard operations like nding the inverse of a matrix, nding its trace, determinant, eigenvalues,
transpose, norm, and how to row reduce the matrix.
Below each function, in comments, is a brief description of how we will implement the function.
module type MATRIX =
3
CS51 Draft Spec April 7, 2013
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
(* Will basically ``flip'' the indices of the input matrix
(* Returns the trace of the matrix *)
val trace: matrix -> elt
(* Will check if the matrix is square, then sum up all the elements along its
* diagonal *)
4
CS51 Draft Spec April 7, 2013
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
from_string : string -> matrix
(* We will have some way to express matrices using strings, and then we will
* parse the string to give the matrix *)
(* Prints out the contents of a matrix *)
print :matrix -> unit
(* Iterate through the matrix and print each element *)
end
The exception exists in case the user tries to perform operations, such as trace or determinant,
which require a square matrix. Matrices have their own type, and they contain elements of the type
elt.
We will probably need a helper function to raise matrices to powers and to solve for zeros of a
polynomial if we want to implement the eigen function.
3.2 Simplex Algorithm
The simplex solves linear programs, which are basically linear inequalities. Given inequalities, using
the simplex algorithm, we can nd the maximum value of a given expression. What the simplex
algorithm does is that it takes in several linear inequalities and writes them so that they are
equalities. For instance 5x + 7y  10 would be 5x + 7y + s = 10 where s  0 is the slack variable.
Then it takes all these equations and puts them a a matrix where each entry represents the coecient
of the variable in the expression. The rst row represents the equation we are optimizing. The nal
column is the constant values of each equation. Then we will do a series of computations so that the
top row has all nonnegative values. This can be done using the functions from the Matrix module.
The steps are as follows:
Look along the rst row and nd the smallest negative element. Look at the column of that entry
and compute the value/element of each row. If any element is zero, we can ignore it. Otherwise take
the element with the smallest value/element and make this the pivot point. Reduce the column
so that the pivot point in 1 and any other entry in the column is zero. Repeat the process again
5
CS51 Draft Spec April 7, 2013
until the top row consists of only nonnegative values. The rst entry in the last column is the
maximum. In addition, for any column consisting of all zeroes and a one, we can nd the value of
the corresponding variable by looking at the value of the row containing the one. If the column
doesn't consists of zeroes and a one, then the value of the corresponding variable is zero.
This algorithm also works with variables that are negative, inequalities where the unknowns is
greater than a constant, and minimum of the equation. This can be easily done by either multiplying
the whole inequality by -1 or by negating the variable. If calculating the minimum or negating the
variable, we can revert them later.
module type simplex
sig
type elt
type linear_equation
type constraint
val pivot : matrix -> matrix
val constraint_from_string : string -> constraint
val linear_from_string : string -> constraint
end
These are helper functions that we think will be used:
no_neg : matrix -> bool
Check the top row has no negative elements, which can be done by using the next helper function
min_of_row : row -> elt*(elt ref)
A helper for the row reduction which will nd the minimum element and its location.
