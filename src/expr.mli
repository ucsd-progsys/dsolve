open Env
open Type
open Predicate


module Exp: sig
  type expr_id = string

  type t =
      Num of int * expr_id
    | Var of string * expr_id
    | Nil of expr_id
    | Cons of t * t * expr_id
    | If of t * t * t * expr_id
    | Match of t * t * (string * string) * t * expr_id
    | Let of string * typ option * t * t * expr_id
    | LetRec of string * typ option * t * t * expr_id
    | Abs of string * typ option * t * expr_id
    | App of t * t * expr_id
        

  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end

module ExpMap: sig
  type key = Exp.t
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

type value =
    NumVal of int
  | ListVal of value list
  | Closure of string * Exp.t * string option * (string * value) list


exception BogusEvalError

val get_next_exp_id: unit -> string

val eval : Exp.t -> value
val expr_get_subexprs: Exp.t -> Exp.t list
val expr_map: (Exp.t -> 'b) -> Exp.t -> 'b list

val expr_to_predicate_expression: Exp.t -> expression

val expr_required_builtin_quals: Exp.t -> qualifier list

val pprint_annotated_expr: (Exp.t -> string) -> int -> Exp.t -> string
val pprint_expr: Exp.t -> string
val pprint_value : value -> string
