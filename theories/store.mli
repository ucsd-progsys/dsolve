type (+'a, +'b) t

val set: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
val get: ('a, 'b) t -> 'a -> 'b
val iter: ('a, 'b) t -> ('a -> 'b -> unit) -> unit
val iteri: ('a, 'b) t -> (int -> 'a -> 'b -> unit) -> unit
val fold: ('a, 'b) t -> ('c -> 'a -> 'b -> 'c) -> 'c -> 'c
val empty: ('a, 'b) t
val init: int -> (int -> 'a) -> (int, 'a) t


