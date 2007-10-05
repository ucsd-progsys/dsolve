open Format

type t = Path.t * Ident.t * Predicate.t

val compare: t -> t -> int
val apply: Ident.t -> t -> Predicate.t

exception Refinement_not_closed

val instantiate: 'a Lightenv.t -> t -> t
val pprint: formatter -> t -> unit
