type typ =
    Arrow of typ * typ
  | Int
  | TyVar of string
  | Nil


val typ_subst_tyvar: typ -> string -> typ -> typ

val pprint_type: typ -> string
