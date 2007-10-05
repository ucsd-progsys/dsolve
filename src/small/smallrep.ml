(* pmr: this one's hard to test as-is, need more debuggin' info *)
qualif LEN(x): x - 1 < n;;
qualif LN(x): x < n;;
qualif LT(x): x < 10;;
qualif LET(x): x - 1 < 10;;

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
