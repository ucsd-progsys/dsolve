open Predicate
open Type
open Frame
open Graph


type subtypconst = SubType of frame Env.t * predicate * frame * frame


module Qualifier: sig
  type t
end


module QualifierSet: sig
  type elt = qualifier
  type t
  val empty : t
  val from_list : elt list -> t
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


val frame_apply_solution: (string -> QualifierSet.t) -> frame -> frame

val constraint_sat: (string -> QualifierSet.t) -> subtypconst -> bool
val solve_constraints: parameterized_pred Env.t -> subtypconst list -> (string -> QualifierSet.t)
