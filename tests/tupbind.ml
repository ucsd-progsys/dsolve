let show x = x
let foo k = (k-2, k-1)
let _     =
  let (p,q) = foo 10 in
  let _     = show (p,q) in 
  assert (p < q) 

let test1 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, i) else assert false in
  let (b,js) = show w in
  (fun j -> assert (b < j)) js

let test2 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, [i]) else assert false in
  let (b,js) = show w in
  List.iter (fun j -> assert (b < j)) js




