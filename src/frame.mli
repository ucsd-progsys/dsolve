open Predicate
open Type


type subst = (string * pexpr) list

type frame =
    FArrow of string * frame * frame
  | FList of frame
  | FVar of subst * string
  | FTyVar of string
  | FInt of subst * qualifier list
  | GenFrame of string list * frame


val pprint_frame: frame -> string
val frame_apply_subst: (string * pexpr) -> frame -> frame

val fresh_frame_from_typ: typ -> frame

val generalize_frame_like_typ: frame -> typ -> frame
val instantiate_frame: frame -> frame

