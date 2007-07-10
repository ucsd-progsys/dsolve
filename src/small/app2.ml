qual P(x): 0 < x;;
qual N(x): x < 0;;

? let inc = fun x -> x + 1 in
let dec = fun y -> y - 1 in
let app = fun f -> fun z -> f z in
let a = app inc 0 in
let b = app dec 0 in
  a;;
