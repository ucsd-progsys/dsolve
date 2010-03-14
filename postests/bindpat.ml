(* (* This works *)
let foo (x,y) = x + y
let z         = foo (2, 3)
let _         = assert (z > 0)
let _         = List.map (fun p -> let x,y = p in x+y) [(2,3)]
*)

(* But this crashes *)
let bar (x,y) = x + y
let _         = List.map bar [(2,3)]


