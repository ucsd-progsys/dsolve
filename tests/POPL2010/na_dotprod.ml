
let dotprod v1 v2 = 
  let rec loop n sum i = 
    if i != n then 
      loop n ((Junkarray2.get v1 i) * (Junkarray2.get v2 i) + sum) (i+1)
    else 
      sum 
  in loop (Junkarray2.length v1) 0 0 

let driver =
  let _  = Random.init 555 in
  let sz = Random.int 40 in
  let v1 = Junkarray2.make sz 1 in
  let v2 = Junkarray2.make sz 1 in
  dotprod v1 v2

