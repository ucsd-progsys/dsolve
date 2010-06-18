type t = A | B of (int -> int -> int)

let foo (x:t) = assert true

let _ = foo A
let _ = foo (B (fun x -> fun y -> x+y))
