open Typedtree

val bind: Env.t -> pattern_desc -> Frame.t -> (Path.t * Frame.t) list
val env_bind: Env.t -> Frame.t Lightenv.t -> pattern_desc -> Frame.t -> Frame.t Lightenv.t
val bind_pexpr: pattern_desc -> Predicate.pexpr -> Frame.substitution list
val same: pattern_desc -> pattern_desc -> bool
