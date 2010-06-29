open Consdef

val pprint_fenv : Format.formatter -> Frame.t Liqenv.t -> unit

val formals_addn: Frame.qvar list -> unit

(* val dsolver: 
  Frame.t Liqenv.t ->
  (Frame.t * labeled_constraint * refinement_constraint) list ->
  Qualifier.t list Sol.t -> 
  Qualifier.t list Sol.t

val solve_with_solver: 
  Parsetree.qualifier_declaration list -> 
  Env.t ->
  int list -> 
  labeled_constraint list ->
  (Frame.t Liqenv.t -> (Frame.t * labeled_constraint * refinement_constraint) list -> Qualifier.t list Sol.t -> Qualifier.t list Sol.t) ->
  ((Frame.qvar -> Qualifier.t list) * (labeled_constraint list))
*)
val solve: 
  string ->
  Env.t ->
  int list -> 
  Parsetree.qualifier_declaration list -> 
  labeled_constraint list ->
  ((Frame.qvar -> Qualifier.t list) * (labeled_constraint list))
