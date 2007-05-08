open Graph
open Expr


type qual = string

type callsite = int

type elabel =
      Call of callsite
    | Return of callsite

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

module VertexSet = Set.Make(Vertex)

module Edge = struct
  type t = elabel option
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = None
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
	None -> []
      | Some(Call i) ->
	  [ `Label("(" ^ string_of_int i) ]
      | Some(Return i) ->
	  [ `Label(")" ^ string_of_int i) ]

  let get_subgraph v =
    None
end

module FlowGraphPrinter = Graphviz.Dot(FlowGraph)


type labelled_qual = QualFrom of qual * (FlowGraph.E.t option)

module LabelledQual = struct
  type t = labelled_qual
  let compare = compare
end


let qual_is_labelled = function
    QualFrom(_, Some(_)) ->
      true
  | _ ->
      false


module LabelledQualSet = Set.Make(LabelledQual)
module SimpleQualMap = Map.Make(FlowGraph.V)

module QualMap = struct
  include SimpleQualMap


  let maplist f qm =
    fold (fun k v r -> (f k v)::r) qm []

  
  let union_disjoint m1 m2 =
    let folder = (fun k d r -> (k, d)::r) in
    let b1 = fold folder m1 [] in
    let b2 = fold folder m2 b1 in
      List.fold_right (fun (k, d) -> add k d) b2 empty
end


let edge_labelled edge =
  match FlowGraph.E.label edge with
      Some(_) ->
	true
    | None ->
	false


let edge_call edge =
  match FlowGraph.E.label edge with
      Some(Call _) ->
	true
    | _ ->
	false


let vertex_quals qmap v =
  try
    QualMap.find v qmap
  with Not_found ->
    LabelledQualSet.empty


let get_edge_source_quals qmap e =
  vertex_quals qmap (FlowGraph.E.src e)


let relabel_quals qualset orig_edge =
  let rec relabel_quals_rec = function
      QualFrom(qual, _)::qs ->
	LabelledQualSet.add (QualFrom(qual, Some orig_edge))
	  (relabel_quals_rec qs)
    | [] ->
	LabelledQualSet.empty
  in
    relabel_quals_rec (LabelledQualSet.elements qualset)


exception InvalidReturnEdge

let unlabel_quals qmap qualset return_edge =
  let j =
    match FlowGraph.E.label return_edge with
	Some(Return n) ->
	  n
      | _ ->
	  raise InvalidReturnEdge
  in      
  let rec unlabel_quals_rec = function
      QualFrom(qual, Some e)::qs ->
	begin match FlowGraph.E.label e with
	    Some(Call i) when i = j ->
	      LabelledQualSet.union
		(get_edge_source_quals qmap e)
		(unlabel_quals_rec qs)
	  | _ ->
	      unlabel_quals_rec qs
	end
    | q::qs ->
	LabelledQualSet.add q (unlabel_quals_rec qs)
    | [] ->
	LabelledQualSet.empty
  in
    unlabel_quals_rec (LabelledQualSet.elements qualset)

(*	
let collect_qualifiers qmap inedges =
  (* Split edges into three classes: call site, return site, regular edges *)
  let (labelled, unlabelled) = List.partition edge_labelled inedges in
  let (call, return) = List.partition edge_call labelled in
  (* Intersect qualifiers that occur along regular edges *)
  let unlabelled_quals =
    try
      List.fold_left
	(fun q e ->
	   LabelledQualSet.inter q (get_edge_source_quals qmap e))
	(get_edge_source_quals qmap (List.hd unlabelled))
	unlabelled
    with Failure _ ->
      LabelledQualSet.empty
  in
  (* Propagate qualifiers from call sites, noting the origin of the qualifier *)
  let call_quals =
    List.fold_left
      (fun q e ->
	 LabelledQualSet.union q
	   (relabel_quals (get_edge_source_quals qmap e) e))
      LabelledQualSet.empty
      call
  in
  (* "Undo" the call for qualifiers at return sites, restoring the qualifiers
     that existed before the call *)
  let return_quals = List.fold_left
    (fun q e ->
       LabelledQualSet.union q
	 (unlabel_quals qmap (get_edge_source_quals qmap e) e))
    LabelledQualSet.empty
    return
  in
    (* Smash all the results together *)
    LabelledQualSet.union
      (LabelledQualSet.union unlabelled_quals call_quals)
      return_quals
*)


let collect_qualifiers qmap inedges combiner =
  try
    List.fold_left
      (fun q e ->
	 combiner q (get_edge_source_quals qmap e))
      (get_edge_source_quals qmap (List.hd inedges))
      inedges
  with _ ->
    LabelledQualSet.empty


let propagate_vertex_qualifiers fg iqmap =
  let rec propagate_vertex_qualifiers_rec qmap qual_combiner = function
      v::w ->
	let new_quals = collect_qualifiers qmap (FlowGraph.pred_e fg v) qual_combiner in
	let orig_quals = vertex_quals iqmap v in
	let qmap' = QualMap.add v (LabelledQualSet.union new_quals orig_quals) qmap in
	let old_quals = vertex_quals qmap v in
	let w' =
	  if not(LabelledQualSet.equal new_quals old_quals) then
	    (FlowGraph.succ fg v)@w
	  else
	    w
	in
	  propagate_vertex_qualifiers_rec qmap' qual_combiner w'
    | [] ->
	qmap
  in
  let all_vertices = FlowGraph.vertices fg in
  let qmap' = propagate_vertex_qualifiers_rec iqmap LabelledQualSet.union all_vertices in
    propagate_vertex_qualifiers_rec qmap' LabelledQualSet.inter all_vertices


let var_vertex varname =
  FlowGraph.V.create (VarName varname)


let expr_vertex e =
  FlowGraph.V.create (ExprId(expr_get_id e))


let get_definite_qual = function
    QualFrom(q, None) ->
      Some q
  | _ ->
      None


let qual_to_str_list =
  Misc.mapfilter get_definite_qual


let expr_quals qmap exp =
  let quals =
    try
      QualMap.find (expr_vertex exp) qmap
    with Not_found ->
      LabelledQualSet.empty
  in
    qual_to_str_list (LabelledQualSet.elements quals)


let qualmap_definite_quals qm =
  let qualset_definite_quals qs =
    Misc.mapfilter get_definite_qual (LabelledQualSet.elements qs)
  in
    QualMap.maplist (fun v qs -> (v, qualset_definite_quals qs)) qm
