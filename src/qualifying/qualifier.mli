open Predicate

type t = Path.t * Ident.t * predicate

val compare: t -> t -> int
val apply: Ident.t -> t -> predicate
val is_well_formed: Ident.t list -> t -> bool
val instantiate: (string * Ident.t) list -> t -> t
