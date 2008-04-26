let f n = ref n 
let decr r = r := !r - 1

let x = f 0
let _ = (fun y -> y) !x
let a = !x 
let x1 = x
(* let _ = decr x1 (* DECRX1 *) *) 
let b = !x 
let xr = f 0
let xr' =  f 10
let _ = decr xr' 

(* 
let _ = assert (a >= 0) (* OK but fails if DECRX1 *)
let _ = assert (!xr >= 0) (* OK   *)
let _ = assert (b >= 0)   (* FAIL *)
let _ = assert (!xr'>= 0) (* FAIL *)
*)

(*************************************************************************)

type mysum = N of int * int | I of int 

let g z = 
  match z with
  | I i -> () (* assert (i >= 0) *)
  | N (i,j) -> ()



