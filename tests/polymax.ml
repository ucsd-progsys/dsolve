let max x y =
  if x < y then y else x

let x = max 1 2
let _ = assert (x > 1)
