val make_t: Frame.t Liqenv.t ->
            Consdef.guard_t ->
            Frame.t ->
            Consdef.refinement ->
            Consdef.simple_refinement ->
            FixConstraint.t

val solver: string ->
            (Frame.t * labeled_constraint * refinement_constraint) list -> 
            Qualifier.t list Sol.t -> 
            Qualifier.t list Sol.t
