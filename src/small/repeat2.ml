? letrec repeat = fun f -> fun n -> fun b -> if n = 0 then b else repeat f (n - 1) (f b) in
let inc = fun x -> x + 1 in
let dec = fun y -> y - 1 in
let g = repeat inc 10 0 in
let h = repeat dec 10 0 in
  g;;
