module type ORDERED_AND_OPERATIONAL =
sig
  type t

  val zero : t

  val one: t

  val compare : t -> t -> Order.order


  (* Converts a t to a string *)
  val to_string : t -> string

  val from_string : string -> t


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

  (* Special test function specifically for float*)
  val generate_x: float -> unit -> t 

end

module Floats : ORDERED_AND_OPERATIONAL =
struct

  type t = float
  
  let epsilon = 0.000001

  let zero = 0.

  let one = 1.

  let compare a b = 
    let diff = (a -. b) /. a in 
    if abs_float diff < epsilon then Order.Equal
    else if a < b then Order.Less
    else Order.Greater
  
  let to_string = string_of_float

  let from_string = float_of_string

  let add = (+.)

  let subtract = (-.)

  let multiply = ( *. ) 

  let divide = (/.) 

  let print a = print_string (to_string a)

  let generate () = 3. 

  let generate_gt a () = a +. 1.

  let generate_lt a () = a -. 1. 

  let generate_between a b () = if a > b then None else Some((a +. b)/. 2.)
  
  let generate_x x () = (x:t)
end
