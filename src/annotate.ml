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
	      (* Printf.printf "\nExamining %s\n" (pprint_expr (fun e -> []) e);
	      Printf.printf "Testing: %s\n" (pprint_predicate test_predicate);
	      Printf.printf "Branch predicate: %s\n" (pprint_predicate be); *)
	      (* pmr: This next expr implies that we're wasting effort by annotating the whole tree
		 per predicate rather than each expression over a set of predicates *)
	      (* We have to do a sanity test here - if be implies false, the branch is unreachable,
		 and we can apply any annotation to any value.  This leads to polluting the context
		 with contradictory assumptions, which again allows us to prove anything for any
		 value... *)
	      if (not (Prover.implies be (Not(True)))) && (Prover.implies be test_predicate) then
		begin
		  let expv = expr_vertex e in
		  let qa = QualFrom(q, None) in
		    (* Printf.printf "True!\n"; *)
		    (* pmr: needs to be moved into a more general place *)
		    try
		      let qs = QualMap.find expv subexp_map in
			QualMap.add expv (LabelledQualSet.add qa qs) subexp_map
		    with Not_found ->
		      QualMap.add expv (LabelledQualSet.singleton qa) subexp_map
		end
	      else
		subexp_map
  in
    annotate_rec qm exp


let fixedpoint_annotate exp predlist =
  let (graph, qmap) = expr_qualgraph exp in
  let all_vertices = (FlowGraph.vertices graph) in
  let rec fixpoint_annotate_rec qm =
    let qm' = List.fold_right (annotate exp) predlist qm in
    let qm'' = propagate_vertex_qualifiers graph qm' all_vertices in
      if qm = qm'' then
	qm''
      else
	let new_predicates = qualmap_to_predicates qm'' predlist in
	  begin
	    List.iter Prover.push new_predicates;
	    fixpoint_annotate_rec qm''
	  end
  in
    fixpoint_annotate_rec qmap
