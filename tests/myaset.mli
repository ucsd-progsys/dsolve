type +'a set

val empty: 'a set
val mem: 'a -> 'a set -> bool
val eq: 'a set -> 'a set -> bool
val of_list: 'a list -> 'a set
val cap: 'a set -> 'a set -> 'a set
val cup: 'a set -> 'a set -> 'a set
val sng: 'a -> 'a set
val mns: 'a set -> 'a set -> 'a set 

val xtr: 'a set -> 'a

val un: unit -> bool 
