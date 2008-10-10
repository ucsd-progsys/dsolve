


let g x = x + 1

let m0 = g

let m1 = Mymap.set m0 2 3

let m2 = Mymap.set m1 3 4

let rec store n m =
  if n = 100 then m else
  let m' = Mymap.set m n (n+1) in
    store (n+1) m'

let _ = store 1 g

let f x = Mymap.sett x 5 6

