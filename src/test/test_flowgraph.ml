open OUnit
open Flowgraph


(* Create a simple graph with v1 l-> v2 *)
let create_simple_graph v1 l v2 =
  let graph = FlowGraph.empty in
  let vgraph = FlowGraph.add_vertex (FlowGraph.add_vertex graph v1) v2 in
  let edge = FlowGraph.E.create v1 l v2 in
    FlowGraph.add_edge_e vgraph edge


let test_regular_flow _ =
  let v1 = FlowGraph.V.create "v1" in
  let v2 = FlowGraph.V.create "v2" in
  let graph = create_simple_graph v1 None v2 in
  let v1_qset = LabelledQualSet.singleton (QualFrom("Q", None)) in
  let qmap = QualMap.add v1 v1_qset QualMap.empty in
  let result = propagate_vertex_qualifiers graph qmap [v1] in
    assert_bool "Did not propagate qualifier across simple edge"
      (LabelledQualSet.equal v1_qset (QualMap.find v2 result))


let suite = "Test Flowgraph" >:::
  ["test_regular_flow" >:: test_regular_flow]
