pred P(x): 0 < x;;
pred N(x): x < 0;;

? let f = fun n ->
  if (fun k -> if z then n else n) true then
    1
  else
    -1
in
let m = f 10 in
let p = f -10 in
  0;;
