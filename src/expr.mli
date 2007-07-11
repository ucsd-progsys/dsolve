open Env
open Type
open Predicate

type expr_id = string

type expr =
    Num of int * expr_id
  | ExpVar of string * expr_id
  | Nil of expr_id
  | Cons of expr * expr * expr_id
  | If of expr * expr * expr * expr_id
  | Let of string * typ option * expr * expr * expr_id
  | LetRec of string * typ option * expr * expr * expr_id
  | Abs of string * typ option * expr * expr_id
  | App of expr * expr * expr_id
  | Cast of typ * typ * expr * expr_id

type value =
    NumVal of int
  | ListVal of value list
  | Closure of string * expr * string option * (string * value) list


exception BogusEvalError


val eval : expr -> value
val expr_get_subexprs: expr -> expr list
val expr_map: (expr -> 'b) -> expr -> 'b list

val expr_to_predicate_expression: expr -> expression

val expr_required_builtin_quals: expr -> qualifier list

val pprint_annotated_expr: (expr -> string) -> int -> expr -> string
val pprint_expr: expr -> string
val pprint_value : value -> string
