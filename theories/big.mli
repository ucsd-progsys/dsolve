type ('a, 'b, 'c) t

val dim1: ('a, 'b, 'c) t -> int
val dim2: ('a, 'b, 'c) t -> int

val set: ('a, 'b, 'c) t -> int ->
  int -> 'a -> unit
val get: ('a, 'b, 'c) t -> int ->
  int -> 'a

val create: ('a, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  int -> int -> ('a, 'b, 'c) t
