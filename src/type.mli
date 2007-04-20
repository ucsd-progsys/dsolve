open Expr


val check_type: expr -> typ -> bool
val infer_type: expr -> typ
val pprint_qual: qual -> string
val pprint_type: typ -> string

