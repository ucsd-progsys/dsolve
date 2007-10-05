qualif LN(x): x - 1 < n;;
qualif LT(x): x - 1 < 10;;

let rec filter f n =
  if n = 0 then
    n
  else
    let k = n - 1 in
    let r = f n in
      if 0 < r then
        let z = filter f k in
          1 + z
      else
        filter f k
in
let t s = s in
  filter t 10;;
  
