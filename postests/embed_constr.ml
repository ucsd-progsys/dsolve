type wrapped_int = Nil | Wrap of int

let a = Nil
let b = Nil
let _ = assert (a = b)

let c = 3
let z = Wrap c
let _ = assert (z = Wrap 3)

let d = 3
let y = Wrap d
let _ = assert (y = Wrap c)

let _ = assert (not (a = y))
