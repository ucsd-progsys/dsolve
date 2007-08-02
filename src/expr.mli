open Env
open Type
open Predicate
open Parsetree


module Expression: sig
  type t = expression
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end


module ExpMap: sig
  type key = Expression.t
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


exception ExpressionNotSupported

val expression_to_pexpr: expression -> pexpr

val expression_required_builtin_quals: expression -> qualifier list

val pprint_annotated_expression: (expression -> string) -> int -> expression -> string
val pprint_expression: expression -> string
