open Predicate
open TheoremProver


type qualifier = string * parameterized_pred

type typ =
    Arrow of string * typ * typ
  | List of typ
  | Int
  | TyVar of string
  | GenTy of string list * typ


val reset_fresh_tyvar: unit -> unit
val fresh_tyvar: unit -> typ

val typ_subst_tyvar: typ -> string -> typ -> typ

val generalize_typ: typ -> typ Env.t -> typ
val instantiate_typ: typ -> typ

val qualify: string -> qualifier -> predicate
val qualifier_subst: pexpr -> string -> qualifier -> qualifier
val qualifier_well_formed: string list -> qualifier -> bool

val pprint_typ: typ -> string
val pprint_quals: qualifier list -> string
