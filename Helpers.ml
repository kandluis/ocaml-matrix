let explode (s: string) (space: string) : string list =
  let len = String.length s in
  let loc = ref len in
  let rec get_elt (buffer: string): string =
    let c = String.sub s (!loc - 1) (!loc) in
    if c = space then (loc := !loc - 1; print_string c; buffer)
    else (loc := !loc - 1; get_elt (c ^ buffer)) in
  let rec build (lst: string list) : string list =
    if !loc = 0 then
      lst
    else
      build ((get_elt s)::lst)  in
    build []