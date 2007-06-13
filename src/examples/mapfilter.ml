pred P(x): 5 < x;;

? let mapfilter = fun f -> fun l ->
  match l with
      Nil ->
	Nil
    | Cons(h, t) ->
	let r = ((mapfilter f) t) in
	let x = f h in
	  match x with
	      Some(y) ->
		Cons(y, r)
	    | None ->
		r
in
let pos = fun y ->
  if 5 < y then
    Some(y)
  else
    None
in
  ((mapfilter pos) Cons(3, Cons(10, Nil)));;
