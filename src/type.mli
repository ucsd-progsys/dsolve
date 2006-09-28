open Expr


val check_type: expr -> typschema -> bool
val infer_type: expr -> typschema
val pprint_type: typschema -> string

val ql: qualliteral -> qual
