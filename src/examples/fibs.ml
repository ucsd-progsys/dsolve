pred P(x): 0 < x;;
pred N(x): x < 0;;

? let fibs = fun n ->
  if n = 0 then
    Nil
  else
    let p = fibs (n - 1) in
      if n <= 2 then
	Cons(1, p)
      else
	match p with
	    Cons(r, Cons(s, m)) ->
	      Cons(r + s, p)
  in fibs 10;;
