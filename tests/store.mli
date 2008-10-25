type (+'a, +'b) t

val mem: ('a, 'b) t -> 'a -> bool
val set: ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
val get: ('a, 'b) t -> 'a -> 'b
val iter: ('a, 'b) t -> ('a -> 'b -> unit) -> unit
val iteri: ('a, 'b) t -> (int -> 'a -> 'b -> unit) -> unit
val fold: ('a, 'b) t -> ('a -> 'b -> 'a -> 'a) -> 'a -> 'a
val create: unit -> ('a, 'b) t
