pred ASAFE(x): x < alen;;
pred BSAFE(x): x < blen;;
pred AUNSAFE(x): alen < x;;

? let min = fun x -> fun y -> if x < y then x else y in
let dot = fun u -> fun v -> fun i -> fun n -> fun s ->
  if n < i then
    s
  else
    dot u v (i + 1) n (s + Ref(u, i) + Ref(v, i))
in
  dot a b 0 (min (alen - 1) (blen - 1)) 0;;

