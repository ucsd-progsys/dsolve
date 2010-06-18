let max x y =
  if x < y then y else x

let x = max 1 2
let _ = assert (x > 1)

let f a b c =
  if b < a && c < a then
    let z = max b c in
    let _ = (fun x -> x) z in
    assert (z >= b && z >= c && z < a)
  else ()
