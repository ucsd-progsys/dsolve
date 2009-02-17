let show x  = x
let fresh x = x + 1

let n0 = 0                    
let n1 = fresh n0             
let n2 = fresh n1

let _  = show n0
let _  = show n1
let _  = show n2

let _  = assert (n0 <= n1)
let _  = assert (n1 <= n2)
