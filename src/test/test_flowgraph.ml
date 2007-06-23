open OUnit
open Flowgraph
open Expr


let vertex s =
  FlowGraph.V.create (ExprId(s, ExpVar(s, s)))

let edge v1 l v2 =
  FlowGraph.E.create v1 l v2

let rec make_graph = function
    ([], []) ->
      FlowGraph.empty
  | (v::vs, es) ->
      FlowGraph.add_vertex (make_graph (vs, es)) v
  | ([], e::es) ->
      FlowGraph.add_edge_e (make_graph ([], es)) e


let qualmap =
  List.fold_left (fun qm (v, qs) -> QualMap.add_vertex_quals v qs qm) QualMap.empty 


let qualset =
  fun l -> List.fold_right QualSet.add l QualSet.empty


let flow graph bot qmap =
  let prove v = QualMap.vertex_quals v QualSet.empty qmap in
  let push _ _ = () in
  let pop () = () in
    flow_qualifiers graph prove push pop bot qmap


let test_regular_flow _ =
  let (v1, v2) = (vertex "v1", vertex "v2") in
  let e = edge v1 (Positive, Flow) v2 in
  let graph = make_graph ([v1; v2], [e]) in
  let v1_qset = QualSet.singleton "Q" in
  let qmap = qualmap [(v1, v1_qset)] in
  let result = flow graph v1_qset qmap in
    assert_bool "Did not propagate qualifier across simple edge"
      (QualSet.equal v1_qset (QualMap.vertex_quals v2 QualSet.empty result))


let test_cycle_termination _ =
  let (v1, v2) = (vertex "v1", vertex "v2") in
  let (forward_edge, backward_edge) = (edge v1 (Positive, Flow) v2, edge v2 (Positive, Flow) v1) in
  let graph = make_graph ([v1; v2], [forward_edge; backward_edge]) in
  let v1_qset = QualSet.singleton "Q" in
  let qmap = qualmap [(v1, v1_qset)] in
  let result = flow graph v1_qset qmap in
    assert_bool "Terminated without propagating qualifier in cycle"
      (QualSet.equal v1_qset (QualMap.vertex_quals v2 QualSet.empty result))

(*
let test_cycle_no_backedge _ =
  let (v1, v2) = (vertex "v1", vertex "v2") in
  let (e1, e2) = (edge v1 Flow v2, edge v2 Flow v1) in
  let graph = make_graph ([v1; v2], [e1; e2]) in
  let backedges = find_backedges graph in
    assert_bool "Found backedge in cycle"
      (not (EdgeSet.mem e1 backedges or EdgeSet.mem e2 backedges))


let test_simple_backedge _ =
  let (vin, vhead, vloop) = (vertex "in", vertex "head", vertex "loop") in
  let (in_edge, prop_edge, loop_edge) = (edge vin Flow vhead, edge vhead Flow vloop, edge vloop Flow vhead) in
  let graph = make_graph ([vin; vhead; vloop], [in_edge; prop_edge; loop_edge]) in
  let backedges = find_backedges graph in
    assert_bool "Did not find single backedge in simple cycle"
      (EdgeSet.equal (EdgeSet.singleton loop_edge) backedges)
*)


let test_loop_propagation _ =
  let (vin, vhead, vloop) = (vertex "in", vertex "head", vertex "loop") in
  let (in_edge, prop_edge, loop_edge) = (edge vin (Positive, Flow) vhead,
					 edge vhead (Positive, Flow) vloop,
					 edge vloop (Positive, Flow) vhead) in
  let graph = make_graph ([vin; vhead; vloop], [in_edge; prop_edge; loop_edge]) in
  let vin_qset = qualset ["Q"] in
  let qmap = qualmap [(vin, vin_qset)] in
  let result = flow graph vin_qset qmap in
    assert_bool "Did not propagate qualifier through loop"
      (QualSet.equal vin_qset (QualMap.vertex_quals vhead QualSet.empty result))


(*
let test_single_call _ =
  let (c, f, r) = (vertex "c", vertex "f", vertex "r") in
  let (ce, re) = (edge c (Some(Call 1)) f, edge f (Some(Return 1)) r) in
  let graph = make_graph ([c; f; r], [ce; re]) in
  let c_qset = TimestampedQualSet.singleton (TimedQual("C", None)) in
  let qmap = qualmap [(c, c_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier did not make it across call"
      (TimestampedQualSet.equal c_qset (QualMap.find r result))


let test_serial_call _ =
  let (c1, c2) = (vertex "c1", vertex "c2") in
  let f = vertex "f" in
  let (r1, r2) = (vertex "r1", vertex "r2") in
  let (ce1, ce2) = (edge c1 (Some(Call 1)) c2, edge c2 (Some(Call 2)) f) in
  let (re1, re2) = (edge f (Some(Return 2)) r2, edge r2 (Some(Return 1)) r1) in
  let graph = make_graph ([c1; c2; f; r2; r1], [ce1; ce2; re2; re1]) in
  let c1_qset = TimestampedQualSet.singleton (TimedQual("C1", None)) in
  let qmap = qualmap [(c1, c1_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier did not make it across serial calls"
      (TimestampedQualSet.equal c1_qset (QualMap.find r1 result))


(* Create two function call sites and ensure that the result of each call
   has the proper qualifier *)
let test_multiple_call _ =
  let f = vertex "f" in
  let (c1, c2) = (vertex "c1", vertex "c2") in
  let (ce1, ce2) = (edge c1 (Some(Call 1)) f, edge c2 (Some(Call 2)) f) in
  let (r1, r2) = (vertex "r1", vertex "r2") in
  let (re1, re2) = (edge f (Some(Return 1)) r1, edge f (Some(Return 2)) r2) in
  let graph = make_graph ([c1; c2; f; r1; r2], [ce1; ce2; re1; re2]) in
  let c1_qset = TimestampedQualSet.singleton (TimedQual("C1", None)) in
  let c2_qset = TimestampedQualSet.singleton (TimedQual("C2", None)) in
  let qmap = qualmap [(c1, c1_qset); (c2, c2_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Return edge for crossed calls have incorrect qualifiers"
      ((TimestampedQualSet.equal c1_qset (QualMap.find r1 result))
       && (TimestampedQualSet.equal c2_qset (QualMap.find r2 result)))


let test_qualifying_function _ =
  let (f, c, r) = (vertex "f", vertex "c", vertex "r") in
  let (ce, re) = (edge c (Some(Call 1)) f, edge f (Some(Return 1)) r) in
  let graph = make_graph ([f; c; r], [ce; re]) in
  let f_qset = TimestampedQualSet.singleton (TimedQual("F", None)) in
  let qmap = qualmap [(f, f_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier incorrect propagated out of function"
      (TimestampedQualSet.equal f_qset (QualMap.find r result))
*)


let test_qualifier_intersection _ =
  let (l, r, c) = (vertex "l", vertex "r", vertex "c") in
  let (le, re) = (edge l (Positive, Flow) c, edge r (Positive, Flow) c) in
  let graph = make_graph ([l; r; c], [le; re]) in
  let l_qset = qualset ["Q1"; "Q2"] in
  let r_qset = qualset ["Q1"; "Q3"] in
  let bot = QualSet.union l_qset r_qset in
  let qmap = qualmap [(l, l_qset); (r, r_qset)] in
  let result = flow graph bot qmap in
    assert_bool "Child qualifier not intersection of parents"
      (QualSet.equal (QualSet.inter l_qset r_qset)
	 (QualMap.vertex_quals c QualSet.empty result))


let test_no_depend_propagate _ =
  let (vfrom, vto) = (vertex "from", vertex "to") in
  let e = edge vfrom (Positive, Depend) vto in
  let graph = make_graph ([vfrom; vto], [e]) in
  let from_qset = qualset ["Q"] in
  let qmap = qualmap [(vfrom, from_qset)] in
  let result = flow graph from_qset qmap in
    assert_bool "Propagated across dependency edge"
      (QualSet.is_empty (QualMap.vertex_quals vto QualSet.empty result))

(*
let test_two_scc_sort _ =
  let (vin, vhead, vloop) = (vertex "in", vertex "head", vertex "loop") in
  let (in_edge, prop_edge, loop_edge) = (edge vin Flow vhead, edge vhead Depend vloop, edge vloop Flow vhead) in
  let graph = make_graph ([vin; vhead; vloop], [in_edge; prop_edge; loop_edge]) in
  let sccs = FlowGraphSCC.scc_list graph in
  let sorted_sccs = sort_sccs graph sccs in
  let sorted =
    match sorted_sccs with
	[[v1]; [v2; v3]] when v1 = vin && v2 != v3 ->
	  true
      | _ ->
	  false
  in
    assert_bool "SCCs out of order" sorted


let test_single_scc_sort _ =
  let (vhead, vloop) = (vertex "head", vertex "loop") in
  let (prop_edge, loop_edge) = (edge vhead Flow vloop, edge vloop Flow vhead) in
  let graph = make_graph ([vhead; vloop], [prop_edge; loop_edge]) in
  let sccs = FlowGraphSCC.scc_list graph in
  let sorted_sccs = sort_sccs graph sccs in
    assert_bool "SCC destroyed magically" (sccs = sorted_sccs)
*)

let suite = "Test Flowgraph" >:::
  ["test_regular_flow" >:: test_regular_flow;
   "test_cycle_termination" >:: test_cycle_termination;
   "test_loop_propagation" >:: test_loop_propagation;
   "test_qualifier_intersection" >:: test_qualifier_intersection;
   "test_no_depend_propagate" >:: test_no_depend_propagate]
(*   "test_two_scc_sort" >:: test_two_scc_sort;
   "test_single_scc_sort" >:: test_single_scc_sort] *)
