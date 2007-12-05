open Types
open Typedtree
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
  | Farrow of pattern_desc option * t * t
  | Ftuple of t list
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

val pprint: formatter -> t -> unit
val pprint_sub: formatter -> substitution -> unit
val pprint_refinement: formatter -> refinement -> unit
val pprint_qualifier_expr: formatter -> qualifier_expr -> unit
val fresh: expression -> t
val fresh_without_vars: expression -> t
val fresh_with_labels: expression -> t -> t
val instantiate: t -> t -> t
val apply_substitution: substitution -> t -> t
val label_like: t -> t -> t
val apply_solution: (Path.t -> Qualifier.t list) -> t -> t
val refinement_conjuncts:
  (Path.t -> Qualifier.t list) -> Path.t -> refinement -> Predicate.t list
val refinement_predicate:
  (Path.t -> Qualifier.t list) -> Path.t -> refinement -> Predicate.t
val refinement_vars: t -> Path.t list
val apply_refinement: refinement -> t -> t
val predicate:
  (Path.t -> Qualifier.t list) -> Path.t -> t -> Predicate.t
val conjuncts:
  (Path.t -> Qualifier.t list) -> Path.t -> t -> Predicate.t list
val refinement_well_formed:
  t Lightenv.t -> (Path.t -> Qualifier.t list) -> refinement -> Path.t -> bool
