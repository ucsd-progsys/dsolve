open Predicate
open TheoremProver
open Type
open Frame


type subtypconst = SubType of frame Env.t * predicate * frame * frame


let pprint_constraint (SubType(env, guard, f1, f2)) =
  Printf.sprintf "[%s] %s |- %s <: %s"
    (Env.pprint pprint_frame env) (pprint_predicate guard) (pprint_frame f1) (pprint_frame f2)


let split_constraints constrs =
  let rec flatten_rec flat = function
      SubType(env, guard, FArrow(x, f1, f1'), FArrow(y, f2, f2'))::cs ->
	flatten_rec flat (SubType(env, guard, f2, f1)::SubType(Env.add x f2 env, guard, f1', f2')::cs)
    | SubType(env, guard, FList f1, FList f2)::cs ->
        flatten_rec flat (SubType(env, guard, f1, f2)::cs)
    | SubType(env, guard, FTyVar _, FTyVar _)::cs ->
        flatten_rec flat cs
    | f::cs ->
        flatten_rec (f::flat) cs
    | [] -> flat
  in flatten_rec [] constrs


module Qualifier = struct
  type t = qualifier
  let compare = compare
end


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


let frame_apply_solution solution fr =
  let rec apply_rec = function
      FArrow(x, f, f') ->
        FArrow(x, apply_rec f, apply_rec f')
    | FList f ->
        FList(apply_rec f)
    | FVar(ss, x) ->
        FInt(ss, QualifierSet.elements (solution x))
    | (FTyVar _)
    | (FInt _) as f ->
        f
    | GenFrame(vars, f) ->
        GenFrame(vars, apply_rec f)
  in apply_rec fr


let subst_quals_predicate x subs quals =
  let unsubst = big_and (List.map (qualify x) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst


let frame_predicate solution x = function
    FInt(subs, quals) -> subst_quals_predicate x subs quals
  | FVar(subs, k) -> subst_quals_predicate x subs (QualifierSet.elements (solution k))
  | f -> True


let environment_predicate solution env = big_and (Env.maplist (frame_predicate solution) env)


(* Unique variable to qualify when testing sat, applicability of qualifiers, etc. *)
let qual_test_var = "AA"


let constraint_sat solution (SubType(env, guard, f1, f2)) =
  let envp = environment_predicate solution env in
  let p1 = frame_predicate solution qual_test_var f1 in
  let p2 = frame_predicate solution qual_test_var f2 in
    Prover.implies (big_and [envp; guard; p1]) p2


exception Unsatisfiable


let refine solution quals = function
    SubType(env, guard, f1, FVar(subs, k2)) ->
      let envp = environment_predicate solution env in
      let p1 = frame_predicate solution qual_test_var f1 in
        Prover.push (big_and [envp; guard; p1]);
        (* pmr: WF should be rolled in as its own set of constraints *)
        let subs_dom = List.map (fun (s, _) -> s) subs in
        let env_dom = Env.maplist (fun k _ -> k) env in
        let qual_dom = subs_dom@env_dom in
        let qual_holds q p =
          if not (qualifier_well_formed qual_dom (q, p)) then
            None
          else
            let qp = subst_quals_predicate qual_test_var subs [("q", p)] in
              if Prover.valid qp then
                Some (q, p)
              else
                None
        in
        let qset = QualifierSet.from_list (Env.mapfilter qual_holds quals) in
          Prover.pop();
          Printf.printf "%s has quals: %s\n" k2 (pprint_quals (QualifierSet.elements (QualifierSet.inter qset (solution k2))));
          Solution.add k2 (QualifierSet.inter qset (solution k2)) solution
  | SubType(_, _, _, _) ->
      (* If we have a literal RHS and we're unsatisfied, we're hosed -
         either the LHS is a literal and we can't do anything, or it's
         a var which has been pushed up by other constraints and, again,
         we can't do anything *)
      raise Unsatisfiable


let solve_constraints quals constrs =
  let cs = split_constraints constrs in
  let rec solve_rec solution =
    try
      let unsat_constr = List.find (fun c -> not (constraint_sat solution c)) cs in
        Printf.printf "Solving %s\n\n" (pprint_constraint unsat_constr);
        solve_rec (refine solution quals unsat_constr)
    with Not_found -> solution
  in
    Printf.printf "Constraints:\n\n";
    List.iter (fun c -> Printf.printf "%s\n\n" (pprint_constraint c)) cs;
    (* pmr: pinning down what a qualifier is would make this considerably easier - or rather,
       wouldn't it just be easier to have qualifiers be in an environment all the time? *)
    solve_rec (Solution.create (QualifierSet.from_list (Env.maplist (fun q p -> (q, p)) quals)))
