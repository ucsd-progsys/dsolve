open Typedtree

val is_deep: pattern_desc -> bool
val pattern_descs: pattern list -> pattern_desc list
val bind_vars: pattern_desc -> pattern_desc -> (Ident.t * Ident.t) list
val bind_pexpr: pattern_desc -> Predicate.pexpr -> Frame.substitution list
val desugar_bind: pattern_desc -> Predicate.pexpr -> Predicate.t
val same: pattern_desc -> pattern_desc -> bool
