open TheoremProver
open Predicate
open Expr


(* Precondition: The theorem prover must already have the expression predicate pushed. *)
let rec annotate exp p =
  let subexp_annotated = 
    match exp with
	Num(_, _)
      | TrueExp _
      | FalseExp _
      | ExpVar _ ->
	  exp
      | BinOp(op, e1, e2, id) ->
	  BinOp(op, annotate e1 p, annotate e2 p, id)
      | BinRel(rel, e1, e2, id) ->
	  BinRel(rel, annotate e1 p, annotate e2 p, id)
      | If(e1, e2, e3, id) ->
	  If(annotate e1 p, annotate e2 p, annotate e3 p, id)
      | Annot(q, e, id) ->
	  Annot(q, annotate e p, id)
      | Let(x, t, e1, e2, id) ->
	  Let(x, t, annotate e1 p, annotate e2 p, id)
      | Abs(x, t, e, id) ->
	  Abs(x, t, annotate e p, id)
      | _ ->
	  (* pmr: obviously this needs to be filled in *)
	  exp
  in
    match p with
	PredOver(q, x, r) ->
	  let (ve, be) = (value_var exp, branch_active exp) in
	  let test_predicate = predicate_subst ve x r in
	    (* pmr: We should totally make a -debug flag *)
(*	    Printf.printf "\nExamining %s\n" (pprint_expr exp);
	    Printf.printf "Testing: %s\n" (pprint_predicate test_predicate);
	    Printf.printf "Branch predicate: %s\n" (pprint_predicate be); *)
	    if Prover.implies be test_predicate then
	      begin
(*		Printf.printf "True!\n"; *)
		Annot(Qual(q), subexp_annotated, expr_get_id exp)
	      end
	    else
	      subexp_annotated

	
