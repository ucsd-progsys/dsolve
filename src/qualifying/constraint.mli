type fc_id 

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * guard_t * Frame.t * Frame.t * origin * fc_id
  | WFFrame of Frame.t Lightenv.t * Frame.t * origin * fc_id

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of frame_constraint

val fresh_fc_id : unit -> fc_id 

val solve: 
  Qualifier.t list -> frame_constraint list ->
    ((Common.ComparablePath.t -> Qualifier.t list) * (frame_constraint list))
