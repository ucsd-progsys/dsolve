qualifier P(x) = 0 < x;;
qualifier N(x) = x < 0;;

let id = fun x -> x in
let a = id 3 in
let b = id (-3) in
  a;;
