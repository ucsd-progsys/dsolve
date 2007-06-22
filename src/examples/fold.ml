pred NNEG(x): 0 <= x;;
pred N(x): x < 0;;

? let fold = fun f -> fun l -> fun b ->
  match l with
      Nil ->
	b
    | Cons(c, d) ->
	f c (fold f d b)
in
let add = fun x -> fun y -> x + y in
let n = Cons(1, Cons(2, Nil)) in
  fold add n 0;;
