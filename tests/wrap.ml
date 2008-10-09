
let f (g: int -> 'a) x = g x
(* f  :: [j] (i:int -> 'a[i/j]) -> x:int -> 'a[x/j] *)

let g x = x

let _ = f g
