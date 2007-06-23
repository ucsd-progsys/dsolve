pred P(x): 0 < x;;
pred N(x): x < 0;;

? let mapfilter = fun f -> fun l ->
  match l with
      Nil ->
	Nil
    | Cons(h, t) ->
	let r = mapfilter f t in
	let x = f h in
	  match x with
	      Some(z) ->
		Cons(z, r)
	    | None ->
		r
in
let pos = fun y ->
  if 0 < y then
    Some(y)
  else
    None
in
  mapfilter pos k;;
