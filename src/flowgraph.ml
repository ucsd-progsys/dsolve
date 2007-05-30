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
    let new_quals = QualSet.diff quals old_quals in
    let hist' = (v, new_quals)::hist in
    let vmap' = VertexMap.add v quals vmap in
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


  let dump (vmap, _) =
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


let find_backedges fg =
  EdgeSet.empty


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
    

(*
let find_backedges fg =
  let find_backedges_from head init_edges =
    let rec find_backedges_rec stack v edges =
      if List.mem v stack then
	if not (FlowGraph.V.equal v  head) then
	  (* By design, every backedge is a flow edge - backedges correspond to
	     parameter passing, which is a flow from a value to a parameter (i.e., the
	     paramter is never going to the the target of a dependency edge) *)
	  let vf = FlowGraph.V.label (List.hd stack) in
	  let vt = FlowGraph.V.label v in
	  let h = FlowGraph.V.label head in
	    begin
	      Printf.printf "Backedge: %s -> %s (%s)\n" (label_of_vlabel vf) (label_of_vlabel vt) (label_of_vlabel h);
	  let edge = FlowGraph.E.create (List.hd stack) Flow v in
	    (* pmr: dirty hack to work around the fact that sometimes we generate a backedge
	       for theorem prover edges --- create a regular edge and see if it exists; if not,
	       we found a theorem prover edge and we don't add it *)
	    if FlowGraph.mem_edge_e fg edge then
	      EdgeSet.add edge edges
	    else
	      edges
	    end
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
*)

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
  let unstable_vertices =
    match backflow with
	IgnoreBackedges ->
	  []
      | FlowBackedges ->
	  let backedges = EdgeSet.elements (find_backedges fg) in
	    List.map FlowGraph.E.dst backedges
  in
  let rec flow_qualifiers_rec qmap = function
      v::w ->
(*	let _ =
	  let ins = List.map (fun e -> label_of_vlabel (FlowGraph.V.label (FlowGraph.E.src e))) (FlowGraph.pred_e fg v) in
	    Printf.printf "Inedges for %s: %s\n" (label_of_vlabel (FlowGraph.V.label v)) (Misc.join ins ", ")
	in *)
	let flow_edges = List.filter is_flow_edge (FlowGraph.pred_e fg v) in
(*	let _ =
	  let ins = List.map (fun e -> label_of_vlabel (FlowGraph.V.label (FlowGraph.E.src e))) flow_edges in
	    Printf.printf "Flow for %s: %s\n" (label_of_vlabel (FlowGraph.V.label v)) (Misc.join ins ", ")
	in *)
	let propagation_edges = List.filter (fun e -> not (EdgeSet.mem e excluded_edges)) flow_edges in
(*	let _ =
	  let ins = List.map (fun e -> label_of_vlabel (FlowGraph.V.label (FlowGraph.E.src e))) propagation_edges in
	    Printf.printf "Prop for %s: %s\n" (label_of_vlabel (FlowGraph.V.label v)) (Misc.join ins ", ")
	in *)
	let new_quals = collect_qualifiers qmap propagation_edges in
	let old_quals = QualMap.vertex_quals v qmap in
(*	let base_quals = QualMap.vertex_quals v base_qmap in *)
	let dead_quals =
	  if List.mem v unstable_vertices then
	    QualSet.diff old_quals new_quals
	  else
	    QualSet.empty
	in
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
  FlowGraph.V.create (ExprId(expr_get_id e, e))


let expr_quals qmap exp =
  let quals = QualMap.vertex_quals (expr_vertex exp) qmap in
    QualSet.elements quals
