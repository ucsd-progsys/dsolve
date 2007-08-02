open Parsetree
open Expr
open Type
open Frame
open Predicate
open Constraint


val infer_shapes: expression -> typ ExpMap.t
val infer_frames: expression -> parameterized_pred Env.t -> frame ExpMap.t

