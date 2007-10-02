open Predicate

type t = Path.t * Ident.t * predicate

val compare: t -> t -> int
val apply: Ident.t -> t -> predicate
val is_well_formed: Ident.t list -> t -> bool

exception Refinement_not_closed

val instantiate: 'a Lightenv.t -> t -> t
