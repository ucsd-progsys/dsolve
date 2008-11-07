type (+'a, +'b, +'c) t = A of 'a * 'b | B of 'b | C of 'c | D

let set (x: ('a, 'b, 'c) t) (y: 'a) (z: 'b) (a: 'c) = x

let get (x: ('a, 'b, 'c) t) (y: 'a) (z: 'b) = match x with C b -> b | _ -> assert false

let iter (x: ('a, 'b, 'c) t) (f: ('a -> 'b -> 'c -> unit)) = ()

let iteri (x: ('a, 'b, 'c) t) (f: (int -> 'a -> 'b -> 'c -> unit)) = ()

let fold (x: ('a, 'b, 'c) t) (f: ('d -> 'a -> 'b -> 'c -> 'd)) (k: 'd) = k

let empty = D

let init (x: int) (y: int) (f: (int -> int -> 'a)) = A (5, 6)
