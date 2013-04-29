let explode (s: string) (space: string) : string list =
  let rec build (curr: string) (buffer: string) (lst: string list) : string list =
    let len = String.length curr in
    if len = 0 then buffer::lst
    else 
      let c = String.sub curr (len - 1) 1 in
      if len = 1 then (c ^ buffer)::lst
      else 
        let s' = String.sub curr 0 (len - 1) in
        if c = space then build s' "" (buffer::lst)
        else if c = "\n" || c = "\r" then build s' buffer lst
        else build s' (c ^ buffer) lst in
  build s "" []
