let x = Myset.empty
let y = (fun x -> x) Myset.empty
let _ = assert ((fun x -> x) (Myset.eq x x))
