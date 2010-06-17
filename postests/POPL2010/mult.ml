let rec mult x y = 
  if (x <= 0 || y <= 0) 
  then 0 
  else (x + (mult x (y - 1)))

(*
let _  = assert (100 <= mult 100 100)
*)

let main n = if 0 <= n then assert (n <= mult n n) 
