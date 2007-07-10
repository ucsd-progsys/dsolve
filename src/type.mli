open Predicate
open TheoremProver


type qualifier = string * parameterized_pred

type typ =
    Arrow of string * typ * typ
  | Int of qualifier list
  | TyVar of string
  | GenVar of string

val fresh_tyvar: unit -> typ

val typ_subst_tyvar: typ -> string -> typ -> typ

val generalize_type: typ -> (string * typ) list -> typ
val instantiate_type: typ -> typ

val qualify: string -> qualifier -> predicate
val qualifier_subst: expression -> string -> qualifier -> qualifier

val pprint_quals: qualifier list -> string
val pprint_type: typ -> string
