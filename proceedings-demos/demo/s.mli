type set
val empty: set
val elts: 'a list -> set
val sng:  'a -> set
val mem:  'a -> set -> bool
val eq : set -> set -> bool
val cap: set -> set -> set
val cup: set -> set -> set
val mns: set -> set -> set 
