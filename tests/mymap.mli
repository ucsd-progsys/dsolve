type (+'b, +'a) store

val set : (int -> 'a) -> int -> 'a -> (int -> 'a) 
val get : (int -> 'a) -> int -> 'a
val mem : (int -> 'a) -> int -> bool
val mk  : unit -> (int -> 'a)
val iter: (int -> 'a) -> (int -> 'a -> unit) -> unit

val memt: ('b, 'a) store -> 'b -> bool
val sett: ('b, 'a) store -> 'b -> 'a -> ('b, 'a) store
val gett: ('b, 'a) store -> 'b -> 'a
val itert: ('b, 'a) store -> ('b -> 'a -> unit) -> unit
val foldt: ('b, 'a) store -> ('b -> 'a -> 'c -> 'c) -> 'c -> 'c
val make: 'b -> 'a -> ('b, 'a) store 
val create: int -> ('b, 'a) store 
