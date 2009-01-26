type (+'a, +'b, +'c) t

val set: ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> ('a, 'b, 'c) t
val get: ('a, 'b, 'c) t -> 'a -> 'b -> 'c
val iter: ('a, 'b, 'c) t -> ('a -> 'b -> 'c -> unit) -> unit
val iteri: ('a, 'b, 'c) t -> (int -> 'a -> 'b -> 'c -> unit) -> unit
val fold: ('a, 'b, 'c) t -> ('d -> 'a -> 'b -> 'c -> 'd) -> 'd -> 'd
val empty: ('a, 'b, 'c) t
val init: int -> int -> (int -> int -> 'a) -> (int, int, 'a) t
