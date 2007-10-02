open Predicate
open Frame

type frame_constraint =
  | SubFrame of frame_expr Lightenv.t * predicate * frame_expr * frame_expr
  | WFFrame of frame_expr Lightenv.t * frame_expr

val environment: frame_constraint -> frame_expr Lightenv.t
val frame_apply_solution: Qualifier.t list Lightenv.t -> frame_expr -> frame_expr
val solve_constraints:
  Qualifier.t list -> frame_constraint list -> Qualifier.t list Lightenv.t

