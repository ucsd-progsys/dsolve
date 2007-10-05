qualif P(x): 0 < x;;
qualif NN(x): 0 < x + 1;;

let rec f n =
  if n = 0 then
    1
  else
    let k = f (n - 1) in n + k
in f 3;;
