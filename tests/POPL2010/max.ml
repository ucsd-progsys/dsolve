//CURRENTLY BROKEN

let max2 (a:int) (b:int) = if a >= b then a else b

let x  = read_int ()
let y  = read_int ()
let m2 = max2 x y

let _  = assert (x <= m2)

(* 
let mmax (x:int) (y:int) (z:int) = max2 x y 
let z  = read_int ()
let _  = assert (y <= m2)
let _  = assert (z <= m2) 

let max3 f (x:int) (y:int) (z:int) = f (f x y) z

let m1 = max3 max2 x y z
let _  = assert ((max2 x m1) = m1)
let _  = assert ((max2 x m2) = m2)
*)
