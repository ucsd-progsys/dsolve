open Predicate
open Type


type subst = (string * pexpr) list

type refinementexpr =
    RVar of string
  | RQuals of qualifier list

type refinement = subst * refinementexpr

type frame =
    FArrow of string * frame * frame
  | FList of frame
  | FTyVar of string
  | FInt of refinement


val pprint_refinement: refinement -> string
val pprint_frame: frame -> string
val frame_apply_subst: (string * pexpr) -> frame -> frame

val fresh_frame_from_typ: typ -> frame

val instantiate_frame_like_typ: frame -> typ -> frame

