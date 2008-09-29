type set

val empty: set
val mem: 'a -> set -> bool
val eq:  set ->  set -> bool
val of_list: 'a list -> set
val cap: set -> set -> set
val cup: set -> set -> set
val sng: 'a -> set
val mns: set -> set -> set 
