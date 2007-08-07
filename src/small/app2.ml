qualifier P(x) = 0 < x;;
qualifier N(x) = x < 0;;

let inc x = x + 1 in
let dec y = y - 1 in
let app f z = f z in
let a = app inc 0 in
let b = app dec 0 in
  a;;
