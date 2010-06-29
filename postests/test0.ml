let fresh x = x + 1
let n0 = 0                    
let n1 = fresh n0             
let _  = assert (n0 <= n1)
