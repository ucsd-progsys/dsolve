(* This works *)
let (<~) = (:=)
let w = ref 0
let _ = w <~ !w + 1
let _ = assert (!w >= 0)

(* This fails *)
let y = ref 0
let _ = y := !y
let _ = assert (!y >= 0)

