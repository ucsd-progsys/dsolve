squalif GTA(x): a < x

let test (a: int) b =
  assert(a < b)

let p = (1, 2)
let (c, d) = p
let _ = test c d
