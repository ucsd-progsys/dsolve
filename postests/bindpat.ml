(* This works *)
let foo (x,y) = x + y
let z = foo (2, 3)
let _ = assert (z > 0)

(* But this crashes *)
let _ = List.map foo [(2,3)]

