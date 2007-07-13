qual LX(x): x <= n;;

? letrec filter = fun f -> fun n ->
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
let t = fun s -> s in
  filter t 10;;
  
