open Predicate
open Frame

type frame_constraint =
    SubFrame of frame_expr Lightenv.t * predicate * frame_expr * frame_expr

val frame_apply_solution: Qualifier.t list Lightenv.t -> frame_expr -> frame_expr
(* val constraint_sat: (string -> QualifierSet.t) -> subrefinementconst -> bool
*)
val solve_constraints: frame_constraint list -> Qualifier.t list Lightenv.t

