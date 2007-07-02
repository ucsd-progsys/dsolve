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
  type 'a t
  type key = string
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end


val constraint_sat: QualifierSet.t Solution.t -> subtypconst -> bool
