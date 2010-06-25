(* DSOLVE -bare -dontminemlq *)

let n1 = read_int () 
let n2 = read_int ()
let hz = fun x y -> assert (x > y)
let _  = if n1 > n2 then hz n1 n2 else ()

