open Graph
open Expr


type qual = string


type edge_type =
    Flow
  | Depend


type vlabel =
    ExprId of expr_id
  | VarName of string
  | NonExpr of string

let string_of_vlabel = function
    ExprId s
  | VarName s
  | NonExpr s ->
      s

module Vertex = struct
  type t = vlabel
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = None
end

module Edge = struct
  type t = edge_type
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = Flow
end

module SimpleFlowGraph = Persistent.Digraph.ConcreteLabeled(Vertex)(Edge)

module FlowGraph = struct
  include SimpleFlowGraph

  let vertices g =
    fold_vertex (fun v l -> v::l) g []

  let graph_attributes g =
    []

  let default_vertex_attributes g =
    []

  let default_edge_attributes g =
    []

  let vertex_name v =
    string_of_vlabel (V.label v)

  let vertex_attributes v =
    [ `Label (vertex_name v) ]

  let edge_attributes e =
    match E.label e with
	Flow -> []
      | Depend -> [ `Style `Dashed ]

  let get_subgraph v =
    None
end

module FlowGraphPrinter = Graphviz.Dot(FlowGraph)

module EdgeSet = Set.Make(FlowGraph.E)


let is_flow_edge e =
  match FlowGraph.E.label e with
      Flow ->
	true
    | Depend ->
	false


module Qual = struct
  type t = qual
  let compare = compare
end


module QualSet = Set.Make(Qual)

module DumbVertexMap = Map.Make(FlowGraph.V)

module VertexMap = struct
  include DumbVertexMap


  let maplist f qm =
    fold (fun k v r -> (f k v)::r) qm []
end


module QualMap = struct
  type history = (Vertex.t * QualSet.t) list
  type t = QualSet.t VertexMap.t * history


  let empty = (VertexMap.empty, [])


  let vertex_quals v (vmap, _) =
    try
      VertexMap.find v vmap
    with Not_found ->
      QualSet.empty


  let add_vertex_quals v quals ((vmap, hist) as qmap) =
    let old_quals = vertex_quals v qmap in
    let all_quals = QualSet.union quals old_quals in
    let new_quals = QualSet.diff quals old_quals in
    let hist' = (v, new_quals)::hist in
    let vmap' = VertexMap.add v all_quals vmap in
      (vmap', hist')


  (* pmr: don't export this, I have no idea how to reconcile
     with history...  meant to be used by obsolete_quals only *)
  let remove_vertex_quals v quals vmap =
    let old_quals = vertex_quals v (vmap, []) in
    let new_quals = QualSet.diff old_quals quals in
    let vmap' = VertexMap.add v new_quals vmap in
      vmap'


  let obsolete_quals v quals (vmap, hist) =
    if QualSet.is_empty quals then
      (vmap, hist)
    else
    let rec obsolete_history ordered_hist hist' =
      match ordered_hist with
	[] ->
	  ([], hist')
      | ((v, qs) as h)::hs ->
	  let dead = QualSet.inter qs quals in
	  if QualSet.is_empty dead then
	    obsolete_history hs (h::hist')
	  else
	    let live = QualSet.diff qs dead in
	    let hist'' =
	      if QualSet.is_empty live then
		hist'
	      else
		(v, live)::hist'
	    in
	      ((v, dead)::hs, hist'')
    in
    let ordered_hist = List.rev hist in
    let (dead, hist') = obsolete_history ordered_hist [] in
    let vmap' = List.fold_right (fun (v, qs) -> remove_vertex_quals v qs) dead vmap in
     (vmap', hist')


  let map f (vmap, _) =
    VertexMap.maplist f vmap


  let equal (vmap1, _) (vmap2, _) =
    vmap1 = vmap2
end


let find_backedges fg =
  let find_backedges_from head init_edges =
    let rec find_backedges_rec stack v edges =
      if List.mem v stack then
	if v != head then
	  (* By design, every backedge is a flow edge - backedges correspond to
	     parameter passing, which is a flow from a value to a parameter (i.e., the
	     paramter is never going to the the target of a dependency edge) *)
	  EdgeSet.add (FlowGraph.E.create (List.hd stack) Flow v) edges
	else
	  edges
      else
	let stack' = v::stack in
	let succs = FlowGraph.succ fg v in
	  List.fold_right (find_backedges_rec stack') succs edges
    in
      find_backedges_rec [] head init_edges
  in
  let all_vertices = FlowGraph.vertices fg in
    List.fold_right find_backedges_from all_vertices EdgeSet.empty


let get_edge_source_quals qmap e =
  QualMap.vertex_quals (FlowGraph.E.src e) qmap


type backedge_flow =
    FlowBackedges
  | IgnoreBackedges


let collect_qualifiers qmap inedges =
  try
    List.fold_left
      (fun q e ->
	 QualSet.inter q (get_edge_source_quals qmap e))
      (get_edge_source_quals qmap (List.hd inedges))
      inedges
  with _ ->
    QualSet.empty


let flow_qualifiers fg backflow base_qmap init_qmap =
  let excluded_edges =
    match backflow with
	FlowBackedges ->
	  EdgeSet.empty
      | IgnoreBackedges ->
	  find_backedges fg
  in
  let rec flow_qualifiers_rec qmap = function
      v::w ->
	let flow_edges = List.filter is_flow_edge (FlowGraph.pred_e fg v) in
	let propagation_edges = List.filter (fun e -> not (EdgeSet.mem e excluded_edges)) flow_edges in
	let new_quals = collect_qualifiers qmap propagation_edges in
	let old_quals = QualMap.vertex_quals v qmap in
	let base_quals = QualMap.vertex_quals v base_qmap in
	let dead_quals = QualSet.diff (QualSet.diff old_quals base_quals) new_quals in
	let qmap'' = QualMap.obsolete_quals v dead_quals qmap in
	let qmap' = QualMap.add_vertex_quals v new_quals qmap'' in
	let w' =
	  if not (QualSet.equal new_quals old_quals) then
	    (FlowGraph.succ fg v)@w
	  else
	    w
	in
	  flow_qualifiers_rec qmap' w'
    | [] ->
	qmap
  in
  let all_vertices = FlowGraph.vertices fg in
    flow_qualifiers_rec init_qmap all_vertices


let var_vertex varname =
  FlowGraph.V.create (VarName varname)


let expr_vertex e =
  FlowGraph.V.create (ExprId(expr_get_id e))


let expr_quals qmap exp =
  let quals = QualMap.vertex_quals (expr_vertex exp) qmap in
    QualSet.elements quals
