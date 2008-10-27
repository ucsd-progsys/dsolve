let show x = x

let max p = 
  let ((x:int), (y:int)) = p in
  if x > y then show x else show y 

let _ = 
  let a = read_int () in
  let b = read_int () in
  let c = max (a,b)   in
  ()
  (*assert (c >= a); assert (c >= b) *)

let max_memo = 
  let t = Hash2.create 17 in
  fun (x:int) (y:int) ->
    if Hash2.mem t x y then 
      Hash2.find t x y
    else
      let res = if x > y then x else y in
      Hash2.add t x y res; res

let rec checker z = 
  let a = read_int () in
  let b = read_int () in
  let c = max_memo a b in
  let _ = assert (c >= a) in
  let _ = assert (c >= b) in
  checker z

let _ = checker 3
