val f_of_dpred: Predicate.t -> Ast.pred
val d_of_fpred: Ast.pred -> Predicate.t

val f_of_dexpr: Predicate.pexpr -> Ast.expr
val d_of_fexpr: Ast.expr -> Predicate.pexpr

val sy_of_path: Path.t -> Ast.Symbol.t 
val path_of_sy: Ast.Symbol.t -> Path.t
val str_of_path: Path.t -> string 
