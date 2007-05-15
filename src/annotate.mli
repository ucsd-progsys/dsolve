open Flowgraph
open TheoremProver
open Predicate
open Expr


val annotate: expr -> named_pred -> QualMap.t -> QualMap.t
val fixedpoint_annotate: expr -> named_pred list -> QualMap.t
