qual NNEG(x): 0 <= x;;
qual N(x): x < 0;;
qual P(x): 0 < x;;

? letrec fold = fun f -> fun l -> fun b ->
  match l with
      [] ->
	b
    | c::d ->
	f c (fold f d b)
in
let add = fun x -> fun y -> x + y in
let n = 1::2::[] in
  fold add n 1;;
