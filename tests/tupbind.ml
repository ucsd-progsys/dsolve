let show x = x
(*
let foo k = (k-2, k-1)
let _     =
  let (p,q) = foo 10 in
  let _     = show (p,q) in 
  assert (p < q) 

let test1 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then show (a, i) else assert false in
  let (b,js) = show w in
  (fun j -> assert (b < j)) js

let test2 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, [i]) else assert false in
  let (b,js) = show w in
  List.iter (fun j -> assert (b < j)) js

let test3 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then show (a, [i]) else assert false in
  let (b,js) = w in
  List.iter (fun j -> assert (b < j)) js

*)

type foo = C of int

let test4 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, C i) else assert false in
  let (b,js) = w in
  match js with C j -> assert (b < j)

let test5 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then show (a, C i) else assert false in
  let (b,js) = w in
  match js with C j -> assert (b < j)



