type (+'a, +'b) t
val mem: ('a, 'b) t -> 'a -> bool
val set: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
val get: ('a, 'b) t -> 'a -> 'b
val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val create: int -> ('a, 'b) t 
