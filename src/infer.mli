open Expr
open Type
open Frame
open Predicate
open Constraint


val infer_shapes: Exp.t -> typ ExpMap.t
val infer_frames: Exp.t -> parameterized_pred Env.t -> (Exp.t -> frame)
