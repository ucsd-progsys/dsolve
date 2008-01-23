open Frame

val refinement_well_formed:
  t Lightenv.t -> (Path.t -> Qualifier.t list) -> refinement -> Path.t -> bool
