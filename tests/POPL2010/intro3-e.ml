(* DSOLVE -bare -dontminemlq *)

let f x g = g (x+1) 

let h z y = assert (y > z + 1) 

let n = read_int ()

let _ = if n >= 0 then f n (h n) else ()

