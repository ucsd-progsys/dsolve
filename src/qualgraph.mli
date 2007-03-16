open Flowgraph
open Expr
open Env


val expr_qualgraph: expr -> (FlowGraph.t * LabelledQualSet.t QualMap.t)
