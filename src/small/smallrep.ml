qualifier LEN(x) = x <= n;;
qualifier LN(x) = x < n;;
qualifier LT(x) = x < 10;;
qualifier LET(x) = x <= 10;;

let rec foo n i f c =
  if i < n then
    let d = f i c in
    let k = i + 1 in
      foo n k f d
  else
    c
in
let g s t = s + t in
  foo 10 0 g 0;;
