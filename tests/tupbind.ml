let show x = x
let foo k = (k-2, k-1) 
let (p,q) = foo 10
let _     = show q
let _     = assert (p < q)
