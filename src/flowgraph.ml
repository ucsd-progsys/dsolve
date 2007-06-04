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


  let vertex_quals v bot vmap =
    try
      VertexMap.find v vmap
    with Not_found ->
      bot


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


let flow_qualifiers fg prove push pop bot init_qmap =
  let rec flow_qualifiers_rec qmap = function
      [] ->
	qmap
    | v::vs ->
	let _ = Printf.printf "\nDumping..." in
	let _ = QualMap.dump qmap in
	let flow_edge_quals e =
	  let src = FlowGraph.E.src e in
	    if is_flow_edge e then
	      Some (QualMap.vertex_quals src bot qmap)
	    else
	      None
	in
	let pred_edges = FlowGraph.pred_e fg v in
	let pred_quals = Misc.mapfilter flow_edge_quals pred_edges in
	let flowed_quals =
	  match pred_quals with
	      [] -> QualSet.empty
	    | qs::_ -> List.fold_right QualSet.inter pred_quals qs in
	let proved_quals =
          (* we can only prove facts about expressions and variables, not intermediate nodes *)
	  if is_expr_vertex v then
	    (* push the quals for the expressions on which we depend, prove quals, pop dependencies *)
	    let filter_depend e =
	      let src = FlowGraph.E.src e in
		if not (is_flow_edge e) then
		  Some src
		else
		  None
	    in
	    let vertex_dependencies = Misc.mapfilter filter_depend pred_edges in
	    let _ = List.iter (fun u -> push u (QualMap.vertex_quals u bot qmap)) vertex_dependencies in
	    let proved = prove v in
	    let _ = List.iter (fun _ -> pop ()) vertex_dependencies in
	      proved
	  else
	    QualSet.empty
	in
	let init_quals = QualMap.vertex_quals v QualSet.empty init_qmap in
	let quals = List.fold_right QualSet.union [flowed_quals; proved_quals] init_quals in
	let old_quals = QualMap.vertex_quals v bot qmap in
	let qmap' = QualMap.add_vertex_quals v quals qmap in
	let w' =
	  if not (QualSet.equal quals old_quals) then
	    (FlowGraph.succ fg v)@vs
	  else
	    vs
	in
	  flow_qualifiers_rec qmap' w'
  in
    flow_qualifiers_rec init_qmap (FlowGraph.vertices fg)


let var_vertex varname =
  FlowGraph.V.create (VarName varname)


let expr_vertex e =
  FlowGraph.V.create (ExprId(expr_get_id e, e))


let expr_quals qmap exp =
  let quals = QualMap.vertex_quals (expr_vertex exp) QualSet.empty qmap in
    QualSet.elements quals
