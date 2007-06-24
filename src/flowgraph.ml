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
  let default = (Positive, Flow)
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
	(_, Flow) -> []
      | (_, Depend) -> [ `Style `Dashed ]
      | (p, Call i) ->
	  [ let paren = match p with Negative -> "(" | Positive -> ")" in
	      `Label (paren ^ string_of_int i) ]

  let get_subgraph v =
    None
end

module FlowGraphPrinter = Graphviz.Dot(FlowGraph)

module VertexSet = Set.Make(FlowGraph.V)
module EdgeSet = Set.Make(FlowGraph.E)


let is_flow_edge e =
  match FlowGraph.E.label e with
      (_, Depend) ->
	false
    | _ ->
	true


(* Remove redundant flow edges in the graph. *)
let canonicalize_flowgraph fg =
  let remove_vertex g v =
    match FlowGraph.V.label v with
	NonExpr _ ->
	  begin match (FlowGraph.pred_e g v, FlowGraph.succ_e g v) with
	      ([ein], [eout]) ->
		begin match (FlowGraph.E.label ein, FlowGraph.E.label eout) with
		    ((_, Call _), (_, Call _)) ->
		      None
		  | (((p, Call _) as label), (p', Flow))
		  | ((p', Flow), ((p, Call _) as label)) ->
		      Some (FlowGraph.E.create (FlowGraph.E.src ein) label (FlowGraph.E.dst eout))
		  | _ ->
		      None
		end
	    | _ ->
		None
	  end
      | _ ->
	  None
  in
  let rec canon_rec g = function
      [] ->
	g
    | v::vs ->
	match remove_vertex g v with
	    None ->
	      canon_rec g vs
	  | Some e ->
	      let g'' = FlowGraph.remove_vertex g v in
	      let g' = FlowGraph.add_edge_e g'' e in
		canon_rec g' (FlowGraph.vertices g')
  in
    canon_rec fg (FlowGraph.vertices fg)


module DumbVertexMap = Map.Make(FlowGraph.V)

module VertexMap = struct
  include DumbVertexMap


  let maplist f qm =
    fold (fun k v r -> (f k v)::r) qm []
end



module Qual = struct
  type t = qual
  let compare = compare
end


module QualSet = struct
  module QSet = Set.Make(Qual)


  type t =
      Ghost
    | Quals of QSet.t

  type elt = Qual.t


  let empty = Quals (QSet.empty)


  let ghost = Ghost


  let elements = function
      Ghost -> []
    | Quals s -> QSet.elements s


  let union a b =
    match (a, b) with
	(Ghost, s)
      | (s, Ghost) -> Ghost
      | (Quals c, Quals d) -> Quals (QSet.union c d)


  let inter a b =
    match (a, b) with
	(Ghost, s)
      | (s, Ghost) -> s
      | (Quals c, Quals d) -> Quals (QSet.inter c d)


  let add e s =
    match s with
	Ghost -> Ghost
      | Quals t -> Quals (QSet.add e t)


  let singleton e =
    Quals (QSet.singleton e)


  let equal a b =
    match (a, b) with
	(Ghost, Ghost) -> true
      | (_, Ghost)
      | (Ghost, _) -> false
      | (Quals c, Quals d) -> QSet.equal c d


  let is_empty = function
      Ghost -> false
    | Quals s -> QSet.is_empty s
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
