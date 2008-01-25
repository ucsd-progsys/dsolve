open Constraint

type error

exception Error of Location.t * error
exception Errors of (Location.t * error) list

module LocationMap: sig
  type key = Location.t
  type 'a t
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

val report_error: Format.formatter -> error -> unit
val report_errors: Format.formatter -> (Location.t * error) list -> unit 

val qualify_implementation:
  string -> Frame.t Lightenv.t -> Qualifier.t list -> Typedtree.structure ->
  Frame.t LocationMap.t
val qualgen_nasty_hack: Frame.t Lightenv.t -> Typedtree.structure -> unit
