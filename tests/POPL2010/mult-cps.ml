
let rec mult x y k = if (x <= 0 || y <= 0) then k 0 else mult x (y - 1) (acc x k)
and acc z m r = m (z + r)
and check100 w = assert (100 <= w)
and main () = mult 100 100 check100

(*
let acc z m r = m (z + r)

let rec mult x y k = 
  if (x <= 0 || y <= 0) 
  then k 0 
  else mult x (y - 1) (acc x k)

let check100 w = assert (100 <= w)

let  _ = mult 100 100 check100
*)
