open Types
open Format

type substitution = Ident.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Ident.t                     (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type t =
    Fvar of Ident.t
  | Fconstr of Path.t * t list * refinement
  | Farrow of Ident.t option * t * t

val pprint: formatter -> t -> unit
val fresh: type_expr -> t
val fresh_with_labels: type_expr -> t -> t
val type_structure: type_expr -> string
val instantiate: t -> t -> t
val apply_substitution: substitution -> t -> t
val label_like: t -> t -> t
val apply_solution: Qualifier.t list Lightenv.t -> t -> t
val refinement_predicate:
  Qualifier.t list Lightenv.t -> Ident.t -> refinement -> Predicate.t
val predicate:
  Qualifier.t list Lightenv.t -> Ident.t -> t -> Predicate.t
val refinement_well_formed:
  t Lightenv.t -> Qualifier.t list Lightenv.t -> refinement -> bool
