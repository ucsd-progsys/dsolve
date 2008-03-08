open Typedtree

val is_deep: pattern_desc -> bool
val bind: Env.t -> pattern_desc -> Frame.t -> (Path.t * Frame.t) list
val desugar: (Path.t * Predicate.pexpr) list -> Predicate.pexpr -> pattern_desc -> (Path.t * Predicate.pexpr) list
val identity_desugar: (Path.t * Predicate.pexpr) list -> pattern_desc -> (Path.t * Predicate.pexpr) list
val env_bind: Env.t -> Frame.t Lightenv.t -> pattern_desc -> Frame.t -> Frame.t Lightenv.t
val bind_pexpr: pattern_desc -> Predicate.pexpr -> Frame.substitution list
val desugar_bind: pattern_desc -> Predicate.pexpr -> Predicate.t
val same: pattern_desc -> pattern_desc -> bool
