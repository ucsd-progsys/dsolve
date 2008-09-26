type set

val of_list: 'a list -> set
val cap:  set ->  set ->  set
val cup:  set ->  set ->  set
val mem:  'a ->  set -> bool
val sng: 'a ->  set
val empty:  set
val eq:  set ->  set -> bool
