let xs = []
let xs = 4::[]
let xs = 3::xs
let xs = 2::xs
let xs = 1::xs 

let _  = 
  match xs with x1::t -> (match t with x2::t' ->  assert (x1 < x2))

