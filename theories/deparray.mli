type (+'a, +'b) t

val create : 'b -> (int, 'b) t
val set    : (int, 'b) t -> int -> 'b -> (int, 'b) t
val get    : (int, 'b) t -> int -> 'b
