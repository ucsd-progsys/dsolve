let rec f a b = 
  let r = {ft = a; sd = b} in
  let _ = (fun x -> x) a in
  let _ = (fun x -> x) b in
  let a' = a + 1 in
  let b' = b + 1 in
    if Random.int 100 > 50 then r else
      if Random.int 100 > 25 then f a b' else f a' b' in
f 10 20;;
