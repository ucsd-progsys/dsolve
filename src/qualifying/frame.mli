open Types
open Format

type substitution = Ident.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Ident.t                     (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type frame_expr =
    Fvar of Ident.t
  | Fconstr of Path.t * frame_expr list * refinement
  | Farrow of Ident.t option * frame_expr * frame_expr

val pprint: formatter -> frame_expr -> unit
val fresh: type_expr -> frame_expr
val fresh_with_labels: type_expr -> frame_expr -> frame_expr
val instantiate: frame_expr -> frame_expr -> frame_expr
val apply_substitution: substitution -> frame_expr -> frame_expr
val label_like: frame_expr -> frame_expr -> frame_expr
val apply_solution: Qualifier.t list Lightenv.t -> frame_expr -> frame_expr
val refinement_predicate:
  Qualifier.t list Lightenv.t -> Ident.t -> refinement -> Predicate.predicate
val predicate:
  Qualifier.t list Lightenv.t -> Ident.t -> frame_expr -> Predicate.predicate
val refinement_well_formed:
  frame_expr Lightenv.t -> Qualifier.t list Lightenv.t -> refinement -> bool
