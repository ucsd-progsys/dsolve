open OUnit
open Flowgraph


let vertex s =
  FlowGraph.V.create (ExprId(s))

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
  List.fold_left (fun qm (v, qs) -> QualMap.add v qs qm) QualMap.empty 


let qualset =
  fun l -> List.fold_right LabelledQualSet.add l LabelledQualSet.empty


let test_regular_flow _ =
  let (v1, v2) = (vertex "v1", vertex "v2") in
  let e = edge v1 None v2 in
  let graph = make_graph ([v1; v2], [e]) in
  let v1_qset = LabelledQualSet.singleton (QualFrom("Q", None)) in
  let qmap = qualmap [(v1, v1_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Did not propagate qualifier across simple edge"
      (LabelledQualSet.equal v1_qset (QualMap.find v2 result))


let test_cycle_termination _ =
  let (v1, v2) = (vertex "v1", vertex "v2") in
  let (forward_edge, backward_edge) = (edge v1 None v2, edge v2 None v1) in
  let graph = make_graph ([v1; v2], [forward_edge; backward_edge]) in
  let v1_qset = LabelledQualSet.singleton (QualFrom("Q", None)) in
  let qmap = qualmap [(v1, v1_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Terminated without propagating qualifier in cycle"
      (LabelledQualSet.equal v1_qset (QualMap.find v2 result))


let test_loop_propagation _ =
  let (vin, vhead, vloop) = (vertex "in", vertex "head", vertex "loop") in
  let (in_edge, prop_edge, loop_edge) = (edge vin None vhead, edge vhead None vloop, edge vloop None vhead) in
  let graph = make_graph ([vin; vhead; vloop], [in_edge; prop_edge; loop_edge]) in
  let vin_qset = LabelledQualSet.singleton (QualFrom("Q", None)) in
  let qmap = qualmap [(vin, vin_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Did not propagate qualifier through loop"
      (LabelledQualSet.equal vin_qset (QualMap.find vhead result))

(*
let test_single_call _ =
  let (c, f, r) = (vertex "c", vertex "f", vertex "r") in
  let (ce, re) = (edge c (Some(Call 1)) f, edge f (Some(Return 1)) r) in
  let graph = make_graph ([c; f; r], [ce; re]) in
  let c_qset = LabelledQualSet.singleton (QualFrom("C", None)) in
  let qmap = qualmap [(c, c_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier did not make it across call"
      (LabelledQualSet.equal c_qset (QualMap.find r result))


let test_serial_call _ =
  let (c1, c2) = (vertex "c1", vertex "c2") in
  let f = vertex "f" in
  let (r1, r2) = (vertex "r1", vertex "r2") in
  let (ce1, ce2) = (edge c1 (Some(Call 1)) c2, edge c2 (Some(Call 2)) f) in
  let (re1, re2) = (edge f (Some(Return 2)) r2, edge r2 (Some(Return 1)) r1) in
  let graph = make_graph ([c1; c2; f; r2; r1], [ce1; ce2; re2; re1]) in
  let c1_qset = LabelledQualSet.singleton (QualFrom("C1", None)) in
  let qmap = qualmap [(c1, c1_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier did not make it across serial calls"
      (LabelledQualSet.equal c1_qset (QualMap.find r1 result))


(* Create two function call sites and ensure that the result of each call
   has the proper qualifier *)
let test_multiple_call _ =
  let f = vertex "f" in
  let (c1, c2) = (vertex "c1", vertex "c2") in
  let (ce1, ce2) = (edge c1 (Some(Call 1)) f, edge c2 (Some(Call 2)) f) in
  let (r1, r2) = (vertex "r1", vertex "r2") in
  let (re1, re2) = (edge f (Some(Return 1)) r1, edge f (Some(Return 2)) r2) in
  let graph = make_graph ([c1; c2; f; r1; r2], [ce1; ce2; re1; re2]) in
  let c1_qset = LabelledQualSet.singleton (QualFrom("C1", None)) in
  let c2_qset = LabelledQualSet.singleton (QualFrom("C2", None)) in
  let qmap = qualmap [(c1, c1_qset); (c2, c2_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Return edge for crossed calls have incorrect qualifiers"
      ((LabelledQualSet.equal c1_qset (QualMap.find r1 result))
       && (LabelledQualSet.equal c2_qset (QualMap.find r2 result)))


let test_qualifying_function _ =
  let (f, c, r) = (vertex "f", vertex "c", vertex "r") in
  let (ce, re) = (edge c (Some(Call 1)) f, edge f (Some(Return 1)) r) in
  let graph = make_graph ([f; c; r], [ce; re]) in
  let f_qset = LabelledQualSet.singleton (QualFrom("F", None)) in
  let qmap = qualmap [(f, f_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Qualifier incorrect propagated out of function"
      (LabelledQualSet.equal f_qset (QualMap.find r result))
*)

let test_qualifier_intersection _ =
  let (l, r, c) = (vertex "l", vertex "r", vertex "c") in
  let (le, re) = (edge l None c, edge r None c) in
  let graph = make_graph ([l; r; c], [le; re]) in
  let l_qset = qualset [QualFrom("Q1", None); QualFrom("Q2", None)] in
  let r_qset = qualset [QualFrom("Q1", None); QualFrom("Q3", None)] in
  let qmap = qualmap [(l, l_qset); (r, r_qset)] in
  let result = propagate_vertex_qualifiers graph qmap in
    assert_bool "Child qualifier not intersection of parents"
      (LabelledQualSet.equal (LabelledQualSet.inter l_qset r_qset)
	 (QualMap.find c result))


let suite = "Test Flowgraph" >:::
  ["test_regular_flow" >:: test_regular_flow;
   "test_cycle_termination" >:: test_cycle_termination;
   "test_loop_propagation" >:: test_loop_propagation;
   "test_qualifier_intersection" >:: test_qualifier_intersection]
