type frame_constraint =
  | SubFrame of Frame.frame_expr Lightenv.t * Predicate.predicate * Frame.frame_expr * Frame.frame_expr
  | WFFrame of Frame.frame_expr Lightenv.t * Frame.frame_expr

type refinement_constraint =
  | SubRefinement of Frame.frame_expr Lightenv.t * Predicate.predicate * Frame.refinement * Frame.refinement
  | WFRefinement of Frame.frame_expr Lightenv.t * Frame.refinement

val environment: frame_constraint -> Frame.frame_expr Lightenv.t
val solve_constraints:
  Qualifier.t list -> frame_constraint list -> Qualifier.t list Lightenv.t
