qual ASAFE(x): x < a;;
qual BSAFE(x): x < b;;

? 
let a = 10 in
let b = 5 in
let min = fun x -> fun y -> if x < y then x else y in
letrec dot = fun u -> fun v -> fun i -> fun n -> fun s ->
  if n < i then
    s
  else
    dot u v (i + 2) n (s + i)
in

  dot a b 0 (min (a - 1) (b - 1)) 0;;

