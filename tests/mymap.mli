type ('b, 'a) store

val set: (int -> 'a) -> int -> 'a -> (int -> 'a) 
val get: (int -> 'a) -> int -> 'a

val mem: (int -> 'a) -> int -> bool

val sett: (int, 'a) store -> int -> 'a -> (int, 'a) store
val gett: (int, 'a) store -> int -> 'a
