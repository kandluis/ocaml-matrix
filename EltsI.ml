module type ORDERED_AND_OPERATIONAL =
sig

  (* Exception for from_string. Is raised when from_string is passed something
   * that is not an elt *)
  exception NonElt

  type t

  (* The zero element *)
  val zero : t

  (* The one element *)
  val one: t

  val compare : t -> t -> Order.order

  (* Converts a t to a string *)
  val to_string : t -> string

  (* Converts a string to a t *)
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

  (* Special test function specifically for float. When passed in a float x,
   * generates an abstract float *)
  val generate_x: float -> unit -> t

  (* Special function for testing. Generates a random t between the zero element
   * and the bound (the argument) inclusive *)
  val generate_random : float -> unit -> t 

  val run_tests : int -> unit
  
end
