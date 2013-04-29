module Floats : EltsI.ORDERED_AND_OPERATIONAL =
struct

  type t = float
  
  let epsilon = 0.000001

  let zero = 0.

  let one = 1.

  let compare a b =
    let a', b' = abs_float a, abs_float b in
    if a' < epsilon && b' < epsilon then 
      Order.Equal
    else 
      let diff = (a -. b) /. (max a' b') in 
      if abs_float diff < epsilon then Order.Equal
      else if a < b then Order.Less
      else if a > b then Order.Greater
      else
        raise (Failure "Error in compare.")
  
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

  let generate_random bound () =
    let x = Random.float bound in
    x -. mod_float x epsilon

  (************************ TESTS ********************************)  

  let rec test_compare (times:int) : unit =
    let random t = float_of_int (Random.int t - Random.int t) in
    if times = 0 then ()
    else
      let x, y = random times, random times in
      match compare x y with
      | Order.Equal -> assert(x = y)
      | Order.Greater -> assert(x > y)
      | Order.Less -> assert(x < y)


  let run_tests (times: int) : unit =
    test_compare times ;
    ()
end
