(* PROJECT: OCaml Matrix Library/Simplex
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
 * SEAS Project URL: code.seas.harvard.edu/ocaml-matrix/ocaml-matrix
 * Video URL: 
 *)


module type SIMPLEX =
sig

  type elt

end 

module Simplex(C: EltsI.ORDERED_AND_OPERATIONAL): SIMPLEX =
struct

	module M : (MatrixI.SIMPLEXMATRIX with type elt = C.t) =
  	Matrix.MakeMatrix(C)

	type elt = M.elt	

end
