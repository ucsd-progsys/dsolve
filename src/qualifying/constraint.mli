type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

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

val environment: frame_constraint -> Frame.t Lightenv.t
val solve_constraints:
  Qualifier.t list -> frame_constraint list -> Qualifier.t list Solution.t


exception Unsatisfiable of Qualifier.t list Solution.t
