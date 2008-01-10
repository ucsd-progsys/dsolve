type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t * origin
  | WFFrame of Frame.t Lightenv.t * Frame.t * origin

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of frame_constraint

type subrefinement_constraint =
    Frame.t Lightenv.t * Predicate.t * Frame.refinement * Frame.refinement * origin

type well_formed_refinement_constraint =
    Frame.t Lightenv.t * Frame.refinement * origin

type refinement_constraint =
  | SubRefinement of subrefinement_constraint
  | WFRefinement of well_formed_refinement_constraint

module VarName: sig
  type t = Path.t
end

module Solution: sig
  type key = VarName.t
  type 'a t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length : 'a t -> int
end

exception Unsatisfiable of refinement_constraint list * Qualifier.t list Solution.t

val environment: frame_constraint -> Frame.t Lightenv.t
val solution_map: 'a Solution.t -> (Solution.key -> 'a)
val solve_constraints:
  Qualifier.t list -> frame_constraint list -> Qualifier.t list Solution.t
