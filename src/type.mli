open Predicate
open TheoremProver


type qualifier = string * parameterized_pred

type typ =
    Arrow of string * typ * typ
  | List of typ
  | Int of qualifier list
  | TyVar of string
  | GenVar of string

val fresh_tyvar: unit -> typ

val typ_subst_tyvar: typ -> string -> typ -> typ

val generalize_type: typ -> typ Env.t -> typ
val instantiate_type: typ -> typ

val qualify: string -> qualifier -> predicate
val qualifier_subst: expression -> string -> qualifier -> qualifier
val qualifier_well_formed: string list -> qualifier -> bool

val pprint_quals: qualifier list -> string
val pprint_type: typ -> string
