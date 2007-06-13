pred L(j): 0 <= j;;
pred U(j): j < alen;;

? let find = fun a -> fun x -> fun i ->
  if alen <= i then
    None
  else
    if r = x then
      Some(i)
    else (((find a) x) (i + 1))
  in (((find b) y) 0);;
