(* DSOLVE -bare -dontminemlq *)

let f n k = if n >= 0 then () else k 0 
let g n = assert (n = 0) 
let n = read_int ()
let _ = f n g

