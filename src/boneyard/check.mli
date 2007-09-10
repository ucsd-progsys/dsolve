open Type
open Expr
open Predicate


val check_type: parameterized_pred Env.t -> expr -> typ -> bool
