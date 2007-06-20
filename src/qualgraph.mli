open Flowgraph
open Expr
open Env


val expr_qualgraph: expr -> QualSet.t -> (FlowGraph.t * QualMap.t)
