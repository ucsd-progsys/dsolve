type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

val environment: frame_constraint -> Frame.t Lightenv.t
val solve_constraints:
  Qualifier.t list -> frame_constraint list -> Qualifier.t list Lightenv.t

exception Unsatisfiable
