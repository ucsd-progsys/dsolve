open Predicate
open Frame

type frame_constraint =
    SubFrame of frame_expr Lightenv.t * predicate * frame_expr * frame_expr

module Solution: sig
  val create: 'b -> ('a -> 'b)
  val add: 'a -> 'b -> ('a -> 'b) -> ('a -> 'b)
end

module QualifierSet: sig
    type elt = Qualifier.t
    type t = Set.Make(Qualifier).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val from_list : elt list -> t
end



val frame_apply_solution: (Ident.t -> QualifierSet.t) -> frame_expr -> frame_expr
(*
val constraint_sat: (string -> QualifierSet.t) -> subrefinementconst -> bool
*)
val solve_constraints: Qualifier.t Lightenv.t -> frame_constraint list ->
  (Ident.t -> QualifierSet.t)

