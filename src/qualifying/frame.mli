open Types
open Predicate

type substitution = Ident.t * pexpr

type qualifier_expr =
    Qvar of Ident.t                     (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type frame_desc =
    Fvar
  | Fconstr of Path.t * frame_expr list * refinement
  | Farrow of Ident.t option * frame_expr * frame_expr

and frame_expr = frame_desc ref

val fresh: type_expr -> frame_expr
val instantiate: frame_expr -> frame_expr -> unit
val apply_substitution: substitution -> frame_expr -> frame_expr
val label_like: frame_expr -> frame_expr -> frame_expr
