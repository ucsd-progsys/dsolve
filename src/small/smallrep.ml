qual LEN(x): x <= n;;
qual LN(x): x < n;;

? letrec foo = fun n -> fun i -> fun f -> fun c ->
  if i < n then
    let d = f i c in
    let k = i + 1 in
      foo n k f d
  else
    c
in
let g = fun s -> fun t -> s + t in
  foo 10 0 g 0;;
