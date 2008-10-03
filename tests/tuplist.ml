let show x = x

(*************************** 1 ***************************)
let foo a b = 
  a::b

let _ = foo

let rec build n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo n xs in
    build n' xs'

let _ = 
  let (m,ys) = build 0 [] in
  List.iter (fun y -> assert (m >= show y)) ys

(*************************** 1 ***************************)

let foo2 a b = 
  (a,[])::b

let _ = foo2

let rec build2 n xs = 
  if read_int () > 0 then (n, xs) else
    let n'  = n + 1  in
    let xs' = foo2 n xs in
    build2 n' xs'

let _ = 
  let (m,ys) = build2 0 [] in
  List.iter (fun y -> assert (m >= fst y)) ys

