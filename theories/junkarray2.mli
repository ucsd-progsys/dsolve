type t = int

val length      : t -> int
val set         : t -> int -> 'a -> unit
val get         : t -> int -> 'a 
val unsafe_set  : t -> int -> 'a -> unit 
val unsafe_get  : t -> int -> 'a 
val make        : int -> 'a -> t  
val create      : int -> 'a -> t 
val init        : int -> (int -> 'a) -> t 
val copy        : t -> t 
val map         : ('a -> 'b) -> t -> t 
