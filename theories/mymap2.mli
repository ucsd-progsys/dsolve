val mem : ('a -> 'b) -> 'a -> bool
val set : ('a -> 'b) -> 'a -> 'b -> ('a -> 'b) 
val get : ('a -> 'b) -> 'a -> 'b
val make: 'a -> 'b   -> ('a -> 'b)
val iter: ('a -> 'b) -> ('a -> 'b -> unit) -> unit
