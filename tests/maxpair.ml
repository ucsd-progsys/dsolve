let show x = x

let max p = 
  let ((x:int), (y:int)) = p in
  if x > y then show x else show y 

let _ = 
  let a = read_int () in
  let b = read_int () in
  let c = max (a,b)   in
  assert (c >= a);
  assert (c >= b)
