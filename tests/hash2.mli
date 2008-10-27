type ('a, 'b, 'c) t
val create: int -> ('a, 'b, 'c) t
val mem   : ('a, 'b, 'c) t -> 'a -> 'b -> bool
val add   : ('a, 'b, 'c) t -> 'a -> 'b -> 'c -> unit
val find  : ('a, 'b, 'c) t -> 'a -> 'b -> 'c 
