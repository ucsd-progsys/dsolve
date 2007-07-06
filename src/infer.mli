open Expr
open Type


val infer_types: expr -> qualifier list -> (expr -> typ)
