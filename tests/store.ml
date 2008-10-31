type (+'a, +'b) t = A of 'a | B of 'b | C

let set (x: ('a, 'b) t) (y: 'a) (z: 'b) = x

let get (x: ('a, 'b) t) (y: 'a) = match x with B b -> b | _ -> assert false

let iter (x: ('a, 'b) t) (f: ('a -> 'b -> unit)) = ()

let iteri (x: ('a, 'b) t) (f: (int -> 'a -> 'b -> unit)) = ()

let fold (x: ('a, 'b) t) (f: ('c -> 'a -> 'b -> 'c)) (k: 'c) = k

let empty = C

let init (x: int) (f: (int -> 'a)) = C
