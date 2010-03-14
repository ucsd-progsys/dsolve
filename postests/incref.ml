(* This works *)
let x = ref 0
let _ = assert (!x >= 0)

(* This works *)
let asgn = (:=)
let z = ref 0
let _ = asgn z !z
let _ = assert (!z >= 0)

(* This fails *) 
let y = ref 0
let _ = y := !y
let _ = assert (!y >= 0)

(* This fails *)
let (<~) = (:=)
let w = ref 0
let _ = asgn w !w
let _ = assert (!w >= 0)

