type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t * origin
  | WFFrame of Frame.t Lightenv.t * Frame.t * origin

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of frame_constraint

val solve: 
  Qualifier.t list -> frame_constraint list ->
    ((Common.ComparablePath.t -> Qualifier.t list) * (frame_constraint list))
