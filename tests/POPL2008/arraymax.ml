let max x y =
  if x > y then x else y

let foldn n b f =
  let rec loop i c =
    if i < n then loop (i+1) (f i c) else c in
    loop 0 b

let arraymax a =
  let am l m = max (Array.get a l) m in
    foldn (Array.length a) 0 am

let vec = Array.make (Random.int 40)  max_int
let _ =    arraymax vec
