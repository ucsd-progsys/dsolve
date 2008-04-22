let test (a: int) b = assert(a < b)

let produce c d = (c, d)

let (m, n) = produce 0 1
let _ = test m n
