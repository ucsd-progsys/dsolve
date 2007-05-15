open TheoremProver
open Flowgraph
open Qualgraph
open Predicate
open Expr


(* Precondition: The theorem prover must already have the expression predicate pushed. *)
let annotate exp p qm =
  let rec annotate_rec qmap e =
    let subexp_map =
      List.fold_left annotate_rec qmap (expr_get_subexprs e)
    in
      match p with
	  (q, PredOver(x, r)) ->
	    let (ve, be) = (value_var e, branch_active e) in
	    let test_predicate = predicate_subst ve x r in
	      (* pmr: We should totally make a -debug flag *)
	      (* Printf.printf "\nExamining %s\n" (pprint_annotated_expr (fun e -> []) e);
	      Printf.printf "Testing: %s\n" (pprint_predicate test_predicate);
	      Printf.printf "Branch predicate: %s\n" (pprint_predicate be); *)
	      if Prover.implies be test_predicate then
		begin
		  let expv = expr_vertex e in
		    (* Printf.printf "True!\n"; *)
		    QualMap.add_vertex_quals expv (QualSet.singleton q) subexp_map
		end
	      else
		subexp_map
  in
    annotate_rec qm exp


let fixedpoint_annotate exp predlist =
  let (graph, qmap) = expr_qualgraph exp in
  let rec fixpoint_annotate_rec qm =
    let qm' = List.fold_right (annotate exp) predlist qm in
    let qm'' = flow_qualifiers graph FlowBackedges qm' qm' in
      if QualMap.equal qm qm'' then
	qm''
      else
	let new_predicates = qualmap_to_predicates qm'' predlist in
	  begin
	    List.iter (fun p -> Printf.printf "New pred: %s\n" (pprint_predicate p)) new_predicates;
	    List.iter Prover.push new_predicates;
	    fixpoint_annotate_rec qm''
	  end
  in
    fixpoint_annotate_rec qmap
