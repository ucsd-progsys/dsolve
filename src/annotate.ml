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
	      Printf.printf "\nExamining %s\n" (pprint_annotated_expr (fun e -> []) e);
	      Printf.printf "Testing: %s\n" (pprint_predicate test_predicate);
	      Printf.printf "Branch predicate: %s\n" (pprint_predicate be);
	      if Prover.implies be test_predicate then
		begin
		  let expv = expr_vertex e in
		    Printf.printf "True!\n";
		    QualMap.add_vertex_quals expv (QualSet.singleton q) subexp_map
		end
	      else
		subexp_map
  in
    annotate_rec qm exp


let fixedpoint_annotate exp predlist =
  let (graph, init_qmap) = expr_qualgraph exp in
  let exp_pred = expr_predicate exp in
  let _ = Printf.printf "%s\n\n" (pprint_predicate exp_pred) in
  let rec fixedpoint_annotate_rec qmap =
    let rec fixedpoint_annotate_pass backedge_flow base_qm qm =
      begin
	begin match backedge_flow with
	    IgnoreBackedges ->
	      Printf.printf "\nFORWARD\n"
	  | FlowBackedges ->
	      Printf.printf "\nBACKWARD\n"
	end;
	QualMap.dump qm;
	let qm'' = List.fold_right (annotate exp) predlist qm in
	  begin
	    Printf.printf "Annoted:\n";
	    QualMap.dump qm'';
	    let qm' = flow_qualifiers graph backedge_flow qm'' qm'' in
	      begin
		Printf.printf "Flowed:\n";
		QualMap.dump qm';
		if QualMap.equal qm qm' then
		  qm'
		else
		  let new_predicates = qualmap_to_predicates qm' predlist in
		    begin
		      List.iter (fun p -> Printf.printf "New pred: %s\n" (pprint_predicate p)) new_predicates;
		      List.iter Prover.push new_predicates;
		      fixedpoint_annotate_pass backedge_flow qm'' qm'
		    end
	      end
	  end
      end
    in
    (* Annotate to a fixed point, ignoring backedges *)
    let _ = Prover.reset(); Prover.push(exp_pred) in
    let qmap'' = fixedpoint_annotate_pass IgnoreBackedges qmap qmap in
    (* Annotate to a fixed point, with backedges *)
    let _ = Prover.reset(); Prover.push(exp_pred) in
    let qmap' = fixedpoint_annotate_pass FlowBackedges qmap'' qmap'' in
      (* If we haven't progressed, we're done *)
      if QualMap.equal qmap qmap' then
	qmap
      else
	fixedpoint_annotate_rec qmap'
  in
    fixedpoint_annotate_rec init_qmap
