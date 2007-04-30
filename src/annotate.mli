open Flowgraph
open TheoremProver
open Predicate
open Expr


val annotate: expr -> named_pred -> LabelledQualSet.t QualMap.t -> LabelledQualSet.t QualMap.t
val fixedpoint_annotate: expr -> named_pred list -> LabelledQualSet.t QualMap.t
