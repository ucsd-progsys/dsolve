open Consdef

val pprint_fenv : Format.formatter -> Frame.t Liqenv.t -> unit

val formals_addn: Frame.qvar list -> unit

val solve: 
  string ->
  (Consdef.Sol.s -> unit) ->
  Env.t ->
  int list -> 
  Parsetree.qualifier_declaration list -> 
  labeled_constraint list ->
  (Frame.qvar -> Qualifier.t list) * (labeled_constraint list)
