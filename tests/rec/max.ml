let mymin (x: int) y =
   if x <= y then x else y

let check x y  = 
  let a = mymin 3 4 in
    if a = 3 then () else assert (1=0)
