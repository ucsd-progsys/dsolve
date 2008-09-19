type 'a set

val setof: 'a list -> 'a set
val inter: 'a set -> 'a set -> 'a set
val union: 'a set -> 'a set -> 'a set
val subset: 'a set -> 'a set -> bool
val mem: 'a -> 'a set -> bool
val add: 'a -> 'a set -> 'a set
val card: 'a set -> int
val is_empty: 'a set -> bool
val singleton: 'a -> 'a set
val empty: 'a set
val empty2: 'a set
val eq: 'a set -> 'a set -> bool

val mul: int -> int -> int
