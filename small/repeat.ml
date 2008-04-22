qualif NNEG(x): 0 < x + 1;;

let rec repeat f n b =
  if n = 0 then
    b
  else
    f (repeat f (n - 1) b)
in
let inc x = x + 1 in
let dec y = y - 1 in
let g = repeat inc 10 0 in
let h = repeat dec 10 0 in
  g;;
