open Types
open Format
open Asttypes

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
  | Ftuple of t list
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

val pprint: formatter -> t -> unit
val pprint_sub: formatter -> substitution -> unit
val pprint_refinement: formatter -> refinement -> unit
val pprint_qualifier_expr: formatter -> qualifier_expr -> unit
val fresh: type_expr -> Env.t -> t
val fresh_without_vars: type_expr -> Env.t -> t
val fresh_with_labels: type_expr -> t -> Env.t -> t
val instantiate: t -> t -> t
val apply_substitution: substitution -> t -> t
val label_like: t -> t -> t
val apply_solution: (Path.t -> Qualifier.t list) -> t -> t
val refinement_predicate:
  (Path.t -> Qualifier.t list) -> Path.t -> refinement -> Predicate.t
val refinement_vars: t -> Path.t list
val apply_refinement: refinement -> t -> t
val predicate:
  (Path.t -> Qualifier.t list) -> Path.t -> t -> Predicate.t
val refinement_well_formed:
  t Lightenv.t -> (Path.t -> Qualifier.t list) -> refinement -> Path.t -> bool
