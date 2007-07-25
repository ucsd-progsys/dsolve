qual NONZERO(x): not(x = 0);;
qual POS(x): 0 < x;;
qual NNEG(x): 0 <= x;;

? let abs = fun x -> if x < 0 then (0 - x) else x in
  let trunc = fun i -> fun j ->
    let ai = abs i in
    let aj = abs j in
      if aj <= ai then j else (fun k -> k) aj
  in
  let s = trunc 0 0 in
  let t = trunc 10 1 in
    s;;
