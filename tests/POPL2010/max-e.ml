(* DSOLVE -bare -dontminemlq *)

let max2 (a:int) (b:int) = if a >= b then a else b
let max3 f (x:int) (y:int) (z:int) = f (f x y) y

let z  = read_int ()
let x  = read_int ()
let y  = read_int ()

let m = max3 max2 x y z
let _  = assert (max2 x m = m)  (* VALID   *)
let _  = assert (max2 y m = m)  (* VALID   *)
let _  = assert (max2 z m = m)  (* INVALID *)
