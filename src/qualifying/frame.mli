open Types
open Format

type substitution = Path.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Path.t                      (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

val empty_refinement: refinement

type t =
    Fvar of Path.t
  | Fconstr of Path.t * t list * refinement
  | Farrow of Path.t option * t * t
  | Ftuple of t * t
  | Funknown

val pprint: formatter -> t -> unit
val pprint_refinement: formatter -> refinement -> unit
val pprint_qualifier_expr: formatter -> qualifier_expr -> unit
val fresh: type_expr -> t
val fresh_without_vars: type_expr -> t
val fresh_with_labels: type_expr -> t -> t
val type_structure: Format.formatter -> type_expr -> unit
val instantiate: t -> t -> t
val apply_substitution: substitution -> t -> t
val label_like: t -> t -> t
val apply_solution: Qualifier.t list Lightenv.t -> t -> t
val refinement_predicate:
  Qualifier.t list Lightenv.t -> Path.t -> refinement -> Predicate.t
val refinement_var: t -> Path.t option
val predicate:
  Qualifier.t list Lightenv.t -> Path.t -> t -> Predicate.t
val refinement_well_formed:
  t Lightenv.t -> Qualifier.t list Lightenv.t -> refinement -> Path.t -> bool
