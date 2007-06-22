pred DEC(y): y <= x;;
pred P(x): 0 < x;;
pred N(x): x < 0;;

? let sum = fun x -> fun s ->
  if x <= 0 then
    s
  else
    sum (x - 1) (x + s)
in
  sum 10 5;;
      
