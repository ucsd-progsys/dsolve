open Graph


type qual = string

type callsite = int

type label =
      Call of callsite
    | Return of callsite

module Vertex = struct
  type t = string
end

module Edge = struct
  type t = label option
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = None
end

module FlowGraph = Persistent.Digraph.AbstractLabeled(Vertex)(Edge)


type labelled_qual = QualFrom of qual * (FlowGraph.E.t option)

module LabelledQual = struct
  type t = labelled_qual
  let compare = compare
end

module LabelledQualSet = Set.Make(LabelledQual)
module QualMap = Map.Make(FlowGraph.V)


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


let propagate_vertex_qualifiers fg qmap w =
  let rec propagate_vertex_qualifiers_rec qmap = function
      v::w ->
	let old_quals = vertex_quals qmap v in
	let new_quals = collect_qualifiers qmap (FlowGraph.pred_e fg v) in
	let qmap' = QualMap.add v (LabelledQualSet.union new_quals old_quals)
	  qmap
	in
	let w' =
	  if not(LabelledQualSet.equal new_quals old_quals) then
	    (FlowGraph.succ fg v)@w
	  else
	    w
	in
	  propagate_vertex_qualifiers_rec qmap' w'
    | [] ->
	qmap
  in
    (* To the user, it's easier to provide the nodes to propagate from;
       internally, it works better if we think of the nodes to propagate
       to *)
  let ws = List.flatten(List.map (FlowGraph.succ fg) w) in
    propagate_vertex_qualifiers_rec qmap ws
