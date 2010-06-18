val length:     int -> int
val set:        int -> int -> 'a -> unit
val get:        int -> int -> 'a 
val unsafe_set: int -> int -> 'a -> unit 
val unsafe_get: int -> int -> 'a 
val make:       int -> 'a -> int 
val create:     int -> 'a -> int 
val init:       int -> (int -> 'a) -> int 
val copy:       int -> int 
val map:        ('a -> 'b) -> int -> int
