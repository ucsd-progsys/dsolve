type 'a t = V of 'a | C of int

let s = V 1

let k = match s with V x -> x | C y -> y

let _ = assert (k = 1)
