let show x = x

let xs = (0,0)::[]
let _  = match xs with q::[] -> let (c,d) = q in assert (c >= 0)
let _  = List.iter (fun p -> let (a,b) = p in assert (a >= 0)) xs
let ys = 0::[]
let _  = match ys with y::[] -> assert (y >= 0)

let foo a b = 
  a::b

let _ = foo

let rec jhala n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo n xs in
    jhala n' xs'

let _ = 
  let (m, ys) = jhala 0 [] in 
  let _ = (let bob = 0 in show ys) in
  List.iter (fun y -> assert (m >= y)) ys (* 1 *)

let foo3 a b = 
  a::b

let _ = 
  let zs = foo3 0 [] in
  zs

let foo2 a b = 
  (a,0)::(assert false)

let _ = foo2

let _ = 
  let ys = foo2 0 [] in
  ys

(*
let rec build2 n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo2 n xs in
    build2 n' xs'

let _ = 
  let (m,ys) = build2 0 [] in
  List.iter (fun y -> assert (m >= fst y)) ys (* 2 *)
*)
(*
let foo3 a b =
  let (c,d) = b in 
  assert (a >= c)

let _ = foo3

let _ = 
  let x = 10 in
  let y = 0 in
  let z = 0 in
  foo3 x (y,z)

let foo4 a b = 
  assert (a >= b)

let _ = 
  let x = 10 in
  let y = 0 in
  foo4 x y
  *)
