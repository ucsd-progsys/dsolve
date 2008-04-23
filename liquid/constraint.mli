type fc_id 

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * guard_t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of labeled_constraint

val fresh_fc_id : unit -> fc_id 

val solve: 
  Qualifier.t list -> labeled_constraint list ->
    ((Common.ComparablePath.t -> Qualifier.t list) * (labeled_constraint list))
