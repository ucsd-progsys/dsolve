open Predicate
open TheoremProver
open Type
open Frame


type subtypconst = SubType of frame Env.t * predicate * frame * frame
type subrefinementconst = SubRef of frame Env.t * predicate * refinement * refinement


let pprint_constraint (SubRef(env, guard, r1, r2)) =
  Printf.sprintf "[%s] %s |- %s <: %s"
    (Env.pprint pprint_frame env) (pprint_predicate guard) (pprint_refinement r1) (pprint_refinement r2)


let split_constraints constrs =
  let rec flatten_rec flat = function
    | SubType(env, guard, FArrow(x, f1, f1'), FArrow(y, f2, f2'))::cs ->
	flatten_rec flat (SubType(env, guard, f2, f1)::SubType(Env.add x f2 env, guard, f1', f2')::cs)
    | SubType(env, guard, FList f1, FList f2)::cs ->
        flatten_rec flat (SubType(env, guard, f1, f2)::cs)
    | SubType(env, guard, FTyVar _, FTyVar _)::cs ->
        flatten_rec flat cs
    | SubType(env, guard, FInt r1, FInt r2)::cs ->
        flatten_rec (SubRef(env, guard, r1, r2)::flat) cs
    | [] -> flat
    | SubType(_, _, f1, f2)::cs ->
        failwith (Printf.sprintf "Cannot split constraint %s <: %s\n" (pprint_frame f1) (pprint_frame f2))
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


let refinement_apply_solution solution = function
    (ss, RVar k) -> (ss, RQuals(QualifierSet.elements (solution k)))
  | r -> r

let frame_apply_solution solution fr =
  let rec apply_rec = function
      FArrow(x, f, f') ->
        FArrow(x, apply_rec f, apply_rec f')
    | FList f ->
        FList(apply_rec f)
    | (FTyVar _) as f ->
        f
    | FInt r ->
        FInt(refinement_apply_solution solution r)
  in apply_rec fr


let subst_quals_predicate qual_var subs quals =
  let unsubst = big_and (List.map (qualify qual_var) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst


let refinement_predicate solution qual_var = function
  | (subs, RVar k) -> subst_quals_predicate qual_var subs (QualifierSet.elements (solution k))
  | (subs, RQuals quals) -> subst_quals_predicate qual_var subs quals


let frame_predicate solution qual_var = function
    FInt r -> refinement_predicate solution qual_var r
  | f -> True


let environment_predicate solution env = big_and (Env.maplist (frame_predicate solution) env)


(* Unique variable to qualify when testing sat, applicability of qualifiers, etc. *)
let qual_test_var = "AA"


let constraint_sat solution (SubRef(env, guard, r1, r2)) =
  let envp = environment_predicate solution env in
  let p1 = refinement_predicate solution qual_test_var r1 in
  let p2 = refinement_predicate solution qual_test_var r2 in
    Prover.implies (big_and [envp; guard; p1]) p2


exception Unsatisfiable


let refine solution quals = function
    SubRef(env, guard, r1, (subs, RVar k2)) ->
      let envp = environment_predicate solution env in
      let p1 = refinement_predicate solution qual_test_var r1 in
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
  | SubRef(_, _, _, _) ->
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
