pred NONZERO(x): not(x = 0);;
pred POS(x): 0 < x;;
pred NNEG(x): 0 <= x;;

? let abs = fun x -> if x < 0 then (0 - x) else x in
  let trunc = fun i -> fun j ->
    let ai = abs i in
    let aj = abs j in
      if aj < ai then j else Div(Mul(ai, j), aj)
  in
    3;;
