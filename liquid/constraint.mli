open Consdef

val pprint_fenv : Format.formatter -> Frame.t Liqenv.t -> unit
val formals_addn: Frame.qvar list -> unit
val env_to_empty_refenv: Frame.t Liqenv.t -> Frame.refinement Liqenv.t
val env_to_refenv: Frame.t Liqenv.t -> Frame.refinement Liqenv.t

val dsolver:
  Frame.t Liqenv.t ->
    (Frame.t * labeled_constraint * refinement_constraint) list ->
      Qualifier.t list Sol.t -> Qualifier.t list Sol.t

val solve_with_solver:
  Parsetree.qualifier_declaration list -> Env.t ->
    int list -> labeled_constraint list ->
  (* solver *)
  (Frame.t Liqenv.t ->
    (Frame.t * labeled_constraint * refinement_constraint) list ->
      Qualifier.t list Sol.t -> Qualifier.t list Sol.t) ->
  ((Frame.qvar -> Qualifier.t list) * (labeled_constraint list))


