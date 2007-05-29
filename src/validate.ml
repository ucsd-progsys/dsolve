open Flowgraph
open TheoremProver


let prove_qual v q =


let validate_vertex g qm v =
  let pred_es = List.map is_flow_edge (FlowGraph.pred_e g v) in
  let preds = List.map FlowGraph.E.src pred_es in
  let pred_qsets = List.map (fun u -> QualMap.vertex_quals u qm) preds in
  let pred_quals =
    try
      List.foldr QualSet.inter pred_qsets (List.hd pred_qsets)
    with Not_found ->
      QualSet.empty
  in
  let quals = QualMap.vertex_quals v qm in
    if QualSet.subset pred_quals quals then
      let quals_to_prove = QualSet.diff quals pred_quals in
	List.for_all (prove_qual v) (QualSet.elements quals_to_prove)
    else
      false

