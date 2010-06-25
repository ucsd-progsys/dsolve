(* DSOLVE -bare -dontminemlq *)

let f x g = g (x+1) 
let h y = assert (y>0)

(* INTRO 1 *)
let n1  = read_int () 
let _   = if n1 > 0 then f n1 h else ()

(* INTRO 2 *)
let n2 = read_int ()
let _  = if n2 >= 0 then f n2 h else ()

(* INTRO 3 *)
let hz z y = assert (y>z)
let n3 = read_int () 
let _ = if n3 >= 0 then f n3 (hz n3) else ()

