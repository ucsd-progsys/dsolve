let show (x: int) = x

let f0 (z: unit) = 0

let make_int n =
  if n = 0 then (* show *) (* comment out "show" and assert fails! *)  (f0 ()) else
    n

let check n = 
  assert (n = make_int n)
