open Predicate
open TheoremProver


type qualifier = string * parameterized_pred

type typ =
    Arrow of string * typ * typ
  | Int of qualifier list
  | TyVar of string


val typ_subst_tyvar: typ -> string -> typ -> typ

val type_const_int: qualifier list -> int -> typ

val pprint_type: typ -> string
