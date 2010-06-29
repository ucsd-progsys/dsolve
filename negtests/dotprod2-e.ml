
let dotprod v1 v2 = 
  let sum = ref 0 in
  for i = 0 to Array.length v1 do
    sum := !sum + (Array.get v1 i) * (Array.get v2 i) 
  done;
  !sum

let driver =
  let _   = Random.init 555 in
  let sz  = Random.int 40 in
  let sz' = sz + 1 in
  let v1  = Array.make sz' (Random.int 5) in
  let v2  = Array.make sz' (Random.int 5) in
  dotprod v1 v2
