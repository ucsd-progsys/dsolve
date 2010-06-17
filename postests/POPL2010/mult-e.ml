let rec mult x y = 
  if (x <= 0 || y <= 0) 
  then 0 
  else (x + (mult x (y - 1)))


let _ = assert (600 <= mult 100 5)
let main n = if 0 <= n then assert ((n+1) <= mult n n) 
