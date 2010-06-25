type t = {a: int; b: int}

let test pr =
  assert(pr.a < pr.b)

let x = max_int
let y = x + 1
let p = {a = x; b = y}
let _ = test p
