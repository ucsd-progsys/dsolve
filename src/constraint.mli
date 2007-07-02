open Predicate
open Type
open Graph


type subst = (string * expression) list


type frame =
    FArrow of string * frame * frame
  | FVar of subst * string
  | FInt of subst * qualifier list


type subtypconst = SubType of (string * frame) list * predicate * frame * frame

val frame_to_type: frame -> typ

val fresh_framevar: unit -> frame

val frame_apply_subst: expression -> string -> frame -> frame


module Qualifier: sig
  type t
end


module QualifierSet: sig
  type elt = qualifier
  type t
  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val elements : t -> elt list
  val equal : t -> t -> bool
  val is_empty : t -> bool
end


module Solution: sig
  val create: 'b -> ('a -> 'b)
  val add: 'a -> 'b -> ('a -> 'b) -> ('a -> 'b)
end


val constraint_sat: (string -> QualifierSet.t) -> subtypconst -> bool
