

val f_of_denvt: Frame.t Lightenv.t -> FixConstraint.envt

val f_of_drefcons: Frame.t Lightenv.t -> (Frame.t * 'a *
  Constraint.refinement_constraint) list -> FixConstraint.t list

val f_of_dsol: Qualifier.t list Constraint.Sol.t -> FixConstraint.soln

