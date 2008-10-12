type (+'b, +'a) store

val set: (int -> 'a) -> int -> 'a -> (int -> 'a) 
val get: (int -> 'a) -> int -> 'a

val mem: (int -> 'a) -> int -> bool

val sett: ('b, 'a) store -> 'b -> 'a -> ('b, 'a) store
val gett: ('b, 'a) store -> 'b -> 'a
val make: 'b -> 'a -> ('b, 'a) store
