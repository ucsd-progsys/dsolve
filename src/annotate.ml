open TheoremProver
open Flowgraph
open Qualgraph
open Predicate
open Expr


let predicate_over_vertex v (PredOver(x, r)) =
  let ve = vertex_value_var v in
    predicate_subst ve x r


let prove preds v =
  let prove_pred (q, p) quals =
    let be = vertex_branch_active v in
    let test_predicate = predicate_over_vertex v p in
      (* pmr: We should totally make a -debug flag *)
      Printf.printf "Testing: %s\n" (pprint_predicate test_predicate);
      Printf.printf "Branch predicate: %s\n" (pprint_predicate be);
      if Prover.implies be test_predicate then
	begin
	  Printf.printf "True!\n";
	  QualSet.add q quals
	end
      else
	quals
  in
    List.fold_right prove_pred preds QualSet.empty


let push preds v quals =
  let qual_preds = List.map (fun q -> predicate_over_vertex v (List.assoc q preds)) (QualSet.elements quals) in
  let pred = big_and qual_preds in
    Prover.push pred


let pop () = Prover.pop ()


let annotate exp preds =
  let bot =
    List.fold_right (fun (q, _) qs -> QualSet.add q qs) preds QualSet.empty
  in
  let (graph, init_qmap) = expr_qualgraph exp bot in
  let exp_pred = expr_predicate exp in
  let _ = Prover.push exp_pred in
    flow_qualifiers graph (prove preds) (push preds) pop bot init_qmap
