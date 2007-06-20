pred DEC(y): y <= x;;

? let sum = fun x -> fun s ->
  if x <= 0 then
    s
  else
    sum (x + 1) (x + s)
in
  sum 10 5;;
