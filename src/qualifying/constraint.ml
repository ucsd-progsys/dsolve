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
                (SubFrame(env, guard, f2, f1)::SubFrame(Lightenv.add x f2 env, guard, f1', f2')::cs)
          | (Fconstr(_, [], r1), Fconstr(_, [], r2)) ->
              split_rec (SubRefinement(env, guard, r1, r2)::flat) cs
          | (Fvar, Fvar) ->
              split_rec flat cs
          | _ -> assert false
  in split_rec [] cstrs

module SimpleQualifierSet = Set.Make(Qualifier)

module QualifierSet = struct
  include SimpleQualifierSet

  let from_list quals =
    List.fold_right add quals empty
end

module Solution = struct
  let create base =
    fun _ -> base

  let add k v solution =
    fun k' -> if k = k' then v else solution k'
end

let refinement_apply_solution solution = function
    (ss, Qvar k) -> (ss, Qconst (QualifierSet.elements (solution k)))
  | r -> r

let frame_apply_solution solution fr =
  let rec apply_rec f =
    match !f with
      Farrow(x, f, f') ->
        ref (Farrow(x, apply_rec f, apply_rec f'))
      | Fconstr(path, fl, r) ->
          ref (Fconstr(path, List.map apply_rec fl,
                       refinement_apply_solution solution r))
      | Fvar ->
          f
  in apply_rec fr

let subst_quals_predicate qual_var subs quals =
  let unsubst = big_and (List.map (Qualifier.apply qual_var) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst

let refinement_predicate solution qual_var = function
  | (subs, Qvar k) -> subst_quals_predicate qual_var subs (QualifierSet.elements (solution k))
  | (subs, Qconst quals) -> subst_quals_predicate qual_var subs quals

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

let refine solution qenv = function
    SubRefinement(env, guard, r1, (subs, Qvar k2)) ->
      let envp = environment_predicate solution env in
      let p1 = refinement_predicate solution qual_test_var r1 in
        Prover.push (big_and [envp; guard; p1]);
        (* pmr: WF should be rolled in as its own set of constraints *)
        let subs_dom = List.map (fun (s, _) -> s) subs in
        let env_dom = Lightenv.maplist (fun k _ -> k) env in
        let qual_dom = subs_dom@env_dom in
        let qual_holds _ q =
          if not (Qualifier.is_well_formed qual_dom q) then
            None
          else
            let qp = subst_quals_predicate qual_test_var subs [q] in
              if Prover.valid qp then
                Some q
              else
                None
        in
        let qset = QualifierSet.from_list (Lightenv.mapfilter qual_holds qenv) in
          Prover.pop();
(*          Printf.printf "%s has quals: %s\n" (Ident.name k2) (pprint_quals (QualifierSet.elements (QualifierSet.inter qset (solution k2)))); *)
          Solution.add k2 (QualifierSet.inter qset (solution k2)) solution
  | SubRefinement _ ->
      (* If we have a literal RHS and we're unsatisfied, we're hosed -
         either the LHS is a literal and we can't do anything, or it's
         a var which has been pushed up by other constraints and, again,
         we can't do anything *)
      raise Unsatisfiable

let solve_constraints quals constrs =
  let cs = split constrs in
  let rec solve_rec solution =
    try
      let unsat_constr = List.find (fun c -> not (constraint_sat solution c)) cs in
(*        Printf.printf "Solving %s\n\n" (pprint_constraint unsat_constr); *)
        solve_rec (refine solution quals unsat_constr)
    with Not_found -> solution
  in
(*    Printf.printf "Constraints:\n\n";
    List.iter (fun c -> Printf.printf "%s\n\n" (pprint_constraint c)) cs; *)
    solve_rec (Solution.create (QualifierSet.from_list (Lightenv.maplist (fun _ q -> q) quals)))
