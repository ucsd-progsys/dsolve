open Format

type t = Path.t * Path.t * Predicate.t

val compare: t -> t -> int
val apply: Path.t -> t -> Predicate.t

exception Refinement_not_closed

val instantiate: 'a Lightenv.t -> t -> t
val pprint: formatter -> t -> unit
