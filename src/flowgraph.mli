open Graph
open Expr


type qual = string

type polarity = Positive | Negative

type flowtype =
    Flow
  | Depend
  | Call of int

type flowlabel = polarity * flowtype

type vlabel =
    ExprId of expr_id * expr
  | VarName of string
  | NonExpr of string

module Vertex:
sig
  type t = vlabel
  val compare : 'a -> 'a -> int
  val hash : 'a -> int
  val equal : 'a -> 'a -> bool
  val default : 'a option
end

module Edge :
sig
  type t = flowlabel
  val compare : 'a -> 'a -> int
  val hash : 'a -> int
  val equal : 'a -> 'a -> bool
  val default : flowlabel
end

(* pmr: should these signatures be fixed? *)
module FlowGraph :
  sig
    type t
    module V :
      sig
	type t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = Vertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t
        val compare : t -> t -> int
        type vertex = V.t
        val src : t -> vertex
        val dst : t -> vertex
        type label = Edge.t
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val empty : t
    val add_vertex : t -> vertex -> t
    val remove_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t

    val vertices : t -> vertex list

    val graph_attributes : t -> Graphviz.DotAttributes.graph list
    val default_vertex_attributes : t -> Graphviz.DotAttributes.vertex list
    val default_edge_attributes : t -> Graphviz.DotAttributes.edge list
    val vertex_name : vertex -> string
    val vertex_attributes : vertex -> Graphviz.DotAttributes.vertex list
    val edge_attributes : edge -> Graphviz.DotAttributes.edge list
    val get_subgraph : t -> Graphviz.DotAttributes.subgraph option
  end

module FlowGraphPrinter :
  sig
    val fprint_graph : Format.formatter -> FlowGraph.t -> unit
    val output_graph : Pervasives.out_channel -> FlowGraph.t -> unit
  end

module QualSet :
  sig
    type elt = qual
    type t
    val empty : t
    val ghost : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
    val equal : t -> t -> bool
    val is_empty : t -> bool
  end

module VertexSet:
  sig
    type elt = FlowGraph.V.t
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

module EdgeSet:
  sig
    type elt = FlowGraph.E.t
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end


module QualMap:
  sig
    type t

    val empty: t

    val vertex_quals: FlowGraph.V.t -> QualSet.t -> t -> QualSet.t
    val add_vertex_quals: FlowGraph.V.t -> QualSet.t -> t -> t
    val map: (FlowGraph.V.t -> QualSet.t -> 'a) -> t -> 'a list
    val equal: t -> t -> bool
    val dump: t -> unit
end

val canonicalize_flowgraph : FlowGraph.t -> FlowGraph.t

val flow_qualifiers: FlowGraph.t -> (FlowGraph.V.t -> QualSet.t) -> (FlowGraph.V.t -> QualSet.t -> unit) -> (unit -> unit) -> QualSet.t -> QualMap.t -> QualMap.t

val string_of_vlabel: vlabel -> string
val label_of_vlabel: vlabel -> string

val var_vertex: string -> FlowGraph.V.t
val expr_vertex: expr -> FlowGraph.V.t
val expr_quals: QualMap.t -> expr -> qual list

val is_expr_vertex: FlowGraph.V.t -> bool
val is_flow_edge: FlowGraph.E.t -> bool
