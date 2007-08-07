qualifier P(x) = 0 < x;;
qualifier NN(x) = 0 <= x;;

let rec f n =
  if n = 0 then
    1
  else
    let k = f (n - 1) in n + k
in f 3;;
