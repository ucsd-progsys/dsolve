let show x = x
let m = ()

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
  (a,[])::b

let _ = foo2

let _ = 
  let ys = foo2 0 [] in
  ys

let rec build2 n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo2 n xs in
    build2 n' xs'

let _ = 
  let (m,ys) = build2 0 [] in
  List.iter (fun y -> assert (m >= fst y)) ys (* 2 *)
