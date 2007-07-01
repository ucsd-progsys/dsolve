open Env
open Type
open Predicate


type expr_id = string

type expr =
    Num of int * expr_id
  | ExpVar of string * expr_id
  | If of expr * expr * expr * expr_id
  | Let of string * typ option * expr * expr * expr_id
  | Abs of string * typ option * expr * expr_id
  | App of expr * expr * expr_id


type value =
    NumVal of int
  | Closure of string * expr * string option * (string * value) list


exception BogusEvalError


val eval : expr -> value
val expr_get_id : expr -> expr_id
val expr_get_subexprs: expr -> expr list
val expr_map: (expr -> 'b) -> expr -> 'b list

val expr_to_predicate_expression: expr -> expression

val pprint_annotated_expr: (expr -> string list) -> int -> expr -> string
val pprint_expr: expr -> string
val pprint_value : value -> string
