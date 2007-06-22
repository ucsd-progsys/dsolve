pred P(x): 0 < x;;
pred N(x): x < 0;;
pred NNEG(x): 0 <= x;;

? let f = fun n ->
  if n = 0 then
    n
  else
    n
in
  f 0;;
