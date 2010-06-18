
type t = int

let n = read_int ()

let f1 (x:int) = x + 1
let _ = assert (n <= (f1 n))

let f2 (x:t) = x + 1
let _ = assert (n <= (f2 n))

let f3 (x:t) : t = x + 1
let _ = assert (n <= (f3 n))



