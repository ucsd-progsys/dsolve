open Expr
open Type
open Predicate


val infer_types: expr -> parameterized_pred Env.t -> (expr -> typ)
