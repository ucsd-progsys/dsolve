(* 
qualif POS(_V):   _V >= 0 
qualif LT(_V):    _V < ~A  
qualif LTLen(_V): _V < Array.length ~A 
*)

let show x = ()

(* Recursion *)
let rec sum k =
  if k < 0 then 0 else
    let s = sum (k-1) in
    s + k

let _ = sum


(* Array Bounds *)
let max x y =
  if x > y then x else y

let arraymax1 a = 
  let rec loop i m = 
    if i < Array.length a then 
      let x  = Array.get a i in
      let m' = max m x in
      loop (i+1) m'
    else 
      m 
  in loop 0 0

let _ = arraymax1

(* Higher-Order Functions *)

let foldn n b f =
  let rec loop i c =
    if i < n then loop (i+1) (f i c) else c in
  loop 0 b

let arraymax2 a =
  let am l m = max (Array.get a l) m in
  foldn (Array.length a) 0 am

let arraytest a =
  let vec = Array.make (Random.int 40)  0 in
  let r   = arraymax2 vec in
(* Polymorphism *)
  assert (r >= 0)



(* Misc *)

let (|>) x f = f x

let abs x = 
  if x > 0 then x else (0 - x)

let magnitude xs =
  let rv = 
    xs 
    |> List.map abs
    |> List.fold_left (+) 0 in
  let _  = assert (rv >= 0) in
  rv
