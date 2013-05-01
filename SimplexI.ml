open Matrix
open EltMatrix

module type SIMPLEX =
sig

  val solve : matrix -> elt option
  
  val run_tests : int -> unit

end 

module Simplex: SIMPLEX =
struct

  let solve (m: matrix) : elt option = None

  let run_tests times = ()

end
