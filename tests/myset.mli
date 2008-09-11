type 'a set

val inter: 'a set -> 'a set -> 'a set
val union: 'a set -> 'a set -> 'a set
val subset: 'a set -> 'a set -> bool
val mem: x: 'a -> y: 'a set -> bool
val card: 'a set -> int
val empty: 'a -> 'a set
val is_empty: 'a set -> bool
val singleton: 'a -> 'a set
