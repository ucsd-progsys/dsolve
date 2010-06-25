let make n f = 
  fun i -> if 0 <= i && i < n then f i else assert false 

let get a (i: int) = a i

let set a (i: int) v = fun j -> if i = j then v else a i 

let inc i = i+1
let a0 = make 1000 inc
let _ = a0
let _ = make
let _ = set

