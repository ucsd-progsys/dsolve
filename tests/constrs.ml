type t = V of int | C of int * int

let s =
  if Random.int 10 > 1 then
    V 1
  else
    C(0, 1)

let k = match s with V x -> x | C(y, z) -> z

let _ = assert (k = 1)
