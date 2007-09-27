open Predicate
open TheoremProver
open Frame

type frame_constraint =
    SubFrame of frame_expr Lightenv.t * predicate * frame_expr * frame_expr
type refinement_constraint =
    SubRefinement of frame_expr Lightenv.t * predicate * refinement * refinement

(*
let pprint_constraint (SubRef(env, guard, r1, r2)) =
  Printf.sprintf "[%s] %s |- %s <: %s"
    (Env.pprint pprint_frame env) (pprint_predicate guard) (pprint_refinement r1) (pprint_refinement r2)
*)

let split cstrs =
  let rec split_rec flat = function
    | [] -> flat
    | SubFrame(env, guard, f1, f2) :: cs ->
        match (!f1, !f2) with
          | (Farrow(x, f1, f1'), Farrow(_, f2, f2')) ->
              split_rec flat
                (SubFrame(env, guard, f2, f1)
                 ::SubFrame(Lightenv.add x f2 env, guard, f1', f2')
                 ::cs)
          | (Fconstr(_, [], r1), Fconstr(_, [], r2)) ->
              split_rec (SubRefinement(env, guard, r1, r2)::flat) cs
          | (Fvar, Fvar) ->
              split_rec flat cs
          | _ -> assert false
  in split_rec [] cstrs

let refinement_apply_solution solution = function
    (ss, Qvar (all_quals, k)) ->
      (ss, Qconst (Lightenv.find_default k all_quals solution))
  | r -> r

let frame_apply_solution solution fr =
  let rec apply_rec f =
    match !f with
      | Farrow(x, f, f') ->
          ref (Farrow(x, apply_rec f, apply_rec f'))
      | Fconstr(path, fl, r) ->
          ref (Fconstr(path, List.map apply_rec fl,
                       refinement_apply_solution solution r))
      | Fvar -> f
  in apply_rec fr

let subst_quals_predicate qual_var subs quals =
  let unsubst = big_and (List.map (Qualifier.apply qual_var) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst

(* pmr: can this be merged with apply_solution somehow? *)
let refinement_predicate solution qual_var = function
  | (subs, Qvar (all_quals, k)) ->
      subst_quals_predicate qual_var subs (Lightenv.find_default k all_quals solution)
  | (subs, Qconst quals) ->
      subst_quals_predicate qual_var subs quals

let frame_predicate solution qual_var f =
  match !f with
      Fconstr(_, _, r) -> refinement_predicate solution qual_var r
        (* pmr: need to elementify on constructed types *)
    | f -> True

let environment_predicate solution env =
  big_and (Lightenv.maplist (frame_predicate solution) env)

(* Unique variable to qualify when testing sat, applicability of qualifiers, etc. *)
let qual_test_var = Ident.create "AA"

let constraint_sat solution (SubRefinement(env, guard, r1, r2)) =
  let envp = environment_predicate solution env in
  let p1 = refinement_predicate solution qual_test_var r1 in
  let p2 = refinement_predicate solution qual_test_var r2 in
    Prover.implies (big_and [envp; guard; p1]) p2

exception Unsatisfiable

let refine solution = function
    SubRefinement(env, guard, r1, (subs, Qvar (all_quals, k2))) ->
      let envp = environment_predicate solution env in
      let p1 = refinement_predicate solution qual_test_var r1 in
        Prover.push (big_and [envp; guard; p1]);
        let qual_holds q =
          if Prover.valid (subst_quals_predicate qual_test_var subs [q]) then
            Some q
          else
            None
        in
        let refined_quals =
          Misc.map_filter qual_holds (Lightenv.find_default k2 all_quals solution) in
          Prover.pop();
          Lightenv.add k2 refined_quals solution
  | SubRefinement _ ->
      (* If we have a literal RHS and we're unsatisfied, we're hosed -
         either the LHS is a literal and we can't do anything, or it's
         a var which has been pushed up by other constraints and, again,
         we can't do anything *)
      raise Unsatisfiable

let solve_constraints constrs =
  let cs = split constrs in
  let rec solve_rec solution =
    try
      let unsat_constr = List.find (fun c -> not (constraint_sat solution c)) cs in
        solve_rec (refine solution unsat_constr)
    with Not_found -> solution
  in solve_rec Lightenv.empty
