qualif POS(x): 0 < x
qualif NEG(x): x < 0

type 'a lst = Nil | Cons of 'a * 'a lst

let empty = Nil
let _ = empty

let pos = Cons (1, empty)
let _ = pos
