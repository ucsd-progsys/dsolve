
let dotprod v1 v2 = 
  let sum = ref 0 in
  for i = 0 to Junkarray2.length v1 do
    sum := !sum + (Junkarray2.get v1 i) * (Junkarray2.get v2 i) 
  done;
  !sum

let driver =
  let _   = Random.init 555 in
  let sz  = Random.int 40 in
  let sz' = sz + 1 in
  let v1  = Junkarray2.make sz' (Random.int 5) in
  let v2  = Junkarray2.make sz' (Random.int 5) in
  dotprod v1 v2
