let show x = x
let foo k = (k-2, k-1) 
let (p,q) = foo 10
let _     = assert (p < q)

let _     = 
  match foo 10 with (a, b) -> let _ = b in assert (a < b)
