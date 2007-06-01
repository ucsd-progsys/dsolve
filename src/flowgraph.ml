open Graph
open Expr


type qual = string


type flowlabel =
    Flow
  | Depend
  | Call of int
  | Return of int


type vlabel =
    ExprId of expr_id * expr
  | VarName of string
  | NonExpr of string

let string_of_vlabel = function
    ExprId(s, _)
  | VarName s
  | NonExpr s ->
      s

let label_of_vlabel = function
    ExprId(_, e) ->
      pprint_expr e
  | VarName s
  | NonExpr s ->
      s

let is_expr_vertex = function
    NonExpr _ ->
      false
  | _ ->
      true

module Vertex = struct
  type t = vlabel
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = None
end

module Edge = struct
  type t = flowlabel
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
    [ `Label (label_of_vlabel (V.label v)) ]

  let edge_attributes e =
    match E.label e with
	Flow -> []
      | Depend -> [ `Style `Dashed ]
      | Call i -> [ `Label ("(" ^ string_of_int i) ]
      | Return i -> [ `Label (")" ^ string_of_int i) ]

  let get_subgraph v =
    None
end

module FlowGraphPrinter = Graphviz.Dot(FlowGraph)

module FlowGraphSCC = Components.Make(FlowGraph)

module VertexSet = Set.Make(FlowGraph.V)
module EdgeSet = Set.Make(FlowGraph.E)


let is_flow_edge e =
  match FlowGraph.E.label e with
      Depend ->
	false
    | _ ->
	true


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
  type t = QualSet.t VertexMap.t


  let empty = VertexMap.empty


  let vertex_quals v vmap =
    try
      VertexMap.find v vmap
    with Not_found ->
      QualSet.empty


  let add_vertex_quals v quals vmap =
    VertexMap.add v quals vmap


  let map f vmap =
    VertexMap.maplist f vmap


  let equal vmap1 vmap2 =
    vmap1 = vmap2


  let dump vmap =
    let print_mapping v qs =
      let label = label_of_vlabel (FlowGraph.V.label v) in
	Printf.printf "%s |-> %s\n" label (Misc.join (QualSet.elements qs) ", ")
    in
    VertexMap.iter print_mapping vmap
end


let dfs_fold fg empty f u =
  let rec dfs_fold_rec stack v result =
    if VertexSet.mem v stack then
      result
    else
      let stack' = VertexSet.add v stack in
      let result' = f v result in
      let succs = FlowGraph.succ fg v in
	List.fold_right (dfs_fold_rec stack') succs result'
  in
    dfs_fold_rec VertexSet.empty u empty


let sort_sccs fg sccs =
  let label_vertex v vmap =
    let c =
      try
	VertexMap.find v vmap
      with Not_found ->
	0
    in
      VertexMap.add v (c + 1) vmap
  in
  let label_from_scc scc vmap =
    let v = List.hd scc in
      dfs_fold fg vmap label_vertex v
  in
  let vmap = List.fold_right label_from_scc sccs VertexMap.empty in
  let compare_sccs scc1 scc2 =
    let (v1, v2) = (List.hd scc1, List.hd scc2) in
      compare (VertexMap.find v1 vmap) (VertexMap.find v2 vmap)
  in
    List.sort compare_sccs sccs


let fix_scc fg prove push pop scc_vertices (init_qmap, init_visited) =
  let scc = List.fold_right VertexSet.add scc_vertices VertexSet.empty in
  let _ = Printf.printf "\n\nFixing SCC [%s]...\n" (Misc.join (List.map (fun v -> label_of_vlabel (FlowGraph.V.label v)) scc_vertices) ", ") in
  let rec df_flow stack v (qmap, visited) =
    let _ = Printf.printf "Flowing at %s\n" (label_of_vlabel (FlowGraph.V.label v)) in
    if VertexSet.mem v stack then
      (qmap, visited)
    else
      let visited_flow_edge_quals e =
	let src = FlowGraph.E.src e in
	if (is_flow_edge e) && (VertexSet.mem src visited) then
	  Some (QualMap.vertex_quals src qmap)
	else
	  None
      in
      let pred_edges = FlowGraph.pred_e fg v in
      let pred_quals = Misc.mapfilter visited_flow_edge_quals pred_edges in
      let flowed_quals =
	match pred_quals with
	    [] -> QualSet.empty
	  | _ -> List.fold_right QualSet.inter pred_quals (List.hd pred_quals) in
      let proved_quals =
        (* we can only prove facts about expressions and variables, not intermediate nodes *)
	if is_expr_vertex v then
	  (* push the quals for the expressions on which we depend *)
	  let filter_depend e =
	    let src = FlowGraph.E.src e in
	      if (not (is_flow_edge e)) && (VertexSet.mem src visited) then
		Some src
	      else
		None
	  in
	  let vertex_dependencies = Misc.mapfilter filter_depend pred_edges in
	  let _ = List.iter (fun u -> push u (QualMap.vertex_quals u qmap)) vertex_dependencies in
	  let proved = prove v in
	  let _ = List.iter (fun _ -> pop ()) vertex_dependencies in
	    proved
	else
	  QualSet.empty
      in
      let quals = QualSet.union flowed_quals proved_quals in
      let qmap' = QualMap.add_vertex_quals v quals qmap in
      let visited' = VertexSet.add v visited in
      let stack' = VertexSet.add v stack in
      let scc_succs = List.filter (fun u -> VertexSet.mem u scc) (FlowGraph.succ fg v) in
      let (qmap'', visited'') = List.fold_right (df_flow stack') scc_succs (qmap', visited') in
	(qmap'', visited'')
  in
  let is_base_case v =
    let pred_edges = FlowGraph.pred_e fg v in
    let flows_from_outside_scc e = (is_flow_edge e) && (not (VertexSet.mem (FlowGraph.E.src e) scc)) in
      List.exists flows_from_outside_scc pred_edges
  in
  let base =
    try
      List.find is_base_case scc_vertices
    with Not_found ->
      (* SCCs without outside predecessors are degenerate - it makes no difference what we choose here *)
      (* pmr: we could probably just ignore these altogether *)
      VertexSet.choose scc
  in
  let rec fix_scc_rec qmap visited =
    let (qmap', visited') = df_flow VertexSet.empty base (qmap, visited) in
      if QualMap.equal qmap qmap' then
	(qmap', visited')
      else
	fix_scc_rec qmap' visited'
  in
    fix_scc_rec init_qmap init_visited


let var_vertex varname =
  FlowGraph.V.create (VarName varname)


let expr_vertex e =
  FlowGraph.V.create (ExprId(expr_get_id e, e))


let expr_quals qmap exp =
  let quals = QualMap.vertex_quals (expr_vertex exp) qmap in
    QualSet.elements quals
