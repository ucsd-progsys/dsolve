let show x = x
let foo k = (k-2, k-1)
let _     =
  let (p,q) = foo 10 in
  let _     = show (p,q) in 
  assert (p < q) 
(* let (a,b) = foo 10
let _     = assert (a < b) *)
(*
let _     = 
  match foo 10 with (a, b) -> let _ = b in assert (a < b)
  *)
