open Predicate
open TheoremProver
open Type


type subst = (string * expression) list


type frame =
    FArrow of string * frame * frame
  | FList of frame
  | FVar of subst * string
  | FGenVar of string
  | FInt of subst * qualifier list


let fresh_framevar = Misc.make_get_fresh (fun x -> FVar([], String.uppercase x))


let rec pprint_frame = function
    FArrow(x, f, f') ->
      Printf.sprintf "%s: %s -> %s" x (pprint_frame f) (pprint_frame f')
  | FList f ->
      Printf.sprintf "%s list" (pprint_frame f)
  | FInt(subs, quals) ->
      Printf.sprintf "[%s] %s" (pprint_subst subs) (pprint_quals quals)
  | FVar(subs, x) ->
      Printf.sprintf "[%s] %s" (pprint_subst subs) x
  | FGenVar a ->
      Printf.sprintf "'%s" a
and pprint_subst ss =
  let pprint_mapping (x, pexp) =
    Printf.sprintf "%s -> %s" x (pprint_expression pexp) in
    Misc.join (List.map pprint_mapping ss) "; "


let rec frame_to_type = function
    FArrow(x, f1, f2) ->
      Arrow(x, frame_to_type f1, frame_to_type f2)
  | FList f ->
      List(frame_to_type f)
  | FVar(_, a) ->
      TyVar a
  | FGenVar a ->
      GenVar a
  | FInt(_, quals) ->
      Int quals


let rec type_to_frame = function
    Arrow(x, t1, t2) ->
      FArrow(x, type_to_frame t1, type_to_frame t2)
  | List t ->
      FList(type_to_frame t)
  | TyVar a ->
      FVar([], a)
  | GenVar a ->
      FGenVar a
  | Int quals ->
      FInt([], quals)


let frame_apply_subst pexp x fr =
  let s = (x, pexp) in
  let rec apply_subst_rec = function
      FArrow(y, f, f') ->
	FArrow(y, apply_subst_rec f, apply_subst_rec f')
    | FList f ->
        FList(apply_subst_rec f)
    | FVar(ss, y) ->
	FVar(s::ss, y)
    | FInt(ss, quals) ->
	FInt(s::ss, quals)
    | FGenVar _ ->
        failwith "Cannot apply subst to genvar"
  in
    apply_subst_rec fr


let instantiate_frame fr =
  let rec instantiate_rec vars = function
      FArrow(x, f1, f2) ->
        let (f1', vars'') = instantiate_rec vars f1 in
        let (f2', vars') = instantiate_rec vars'' f2 in
          (FArrow(x, f1', f2'), vars')
    | FList f ->
        let (f', vars') = instantiate_rec vars f in
          (FList f', vars')
    | FGenVar a ->
        let (f', vars') =
          try
            (List.assoc a vars, vars)
          with Not_found ->
            let f = fresh_framevar () in
              (f, (a, f)::vars)
        in
          (f', vars')
    | f ->
        (f, vars)
  in
  let (f, _) = instantiate_rec [] fr in
    f


type subtypconst = SubType of (string * frame) list * predicate * frame * frame


let pprint_env env =
  let pprint_mapping (x, f) =
    Printf.sprintf "%s -> %s" x (pprint_frame f) in
    Misc.join (List.map pprint_mapping env) "; "


let pprint_constraint (SubType(env, guard, f1, f2)) =
  Printf.sprintf "[%s] %s |- %s <: %s"
    (pprint_env env) (pprint_predicate guard) (pprint_frame f1) (pprint_frame f2)


let split_constraints constrs =
  let rec flatten_rec cs flat =
    match cs with
	SubType(env, guard, FArrow(x, f1, f1'), FArrow(y, f2, f2'))::cs ->
	  let param = SubType(env, guard, f2, f1) in
	  let env' = (x, f2)::env in
	  let ret = SubType(env', guard, f1', f2') in
	    flatten_rec (param::ret::cs) flat
      | SubType(env, guard, FList f1, FList f2)::cs ->
          flatten_rec (SubType(env, guard, f1, f2)::cs) flat
      | SubType(_, _, FGenVar _, FGenVar _)::cs ->
          flatten_rec cs flat
      | f::cs ->
	  flatten_rec cs (f::flat)
      | [] ->
	  flat
  in
    flatten_rec constrs []


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
    | (FInt _) as f ->
        f
    | (FGenVar _) as f ->
        f
  in
    apply_rec fr


let subst_quals_predicate x subs quals =
  let unsubst = big_and (List.map (qualify x) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst


let frame_predicate solution (x, f) =
  let (subs, quals) = match f with
      FVar(subst, k) ->
	(subst, QualifierSet.elements (solution k))
    | FInt(subst, qs) ->
	(subst, qs)
    | _ ->
	([], [])
  in
    subst_quals_predicate x subs quals


let environment_predicate solution env =
  big_and (List.map (frame_predicate solution) env)


let constraint_sat solution (SubType(env, guard, f1, f2)) =
  let envp = environment_predicate solution env in
  let p1 = frame_predicate solution ("A", f1) in
  let p2 = frame_predicate solution ("A", f2) in
    Prover.implies (big_and [envp; guard; p1]) p2


exception Unsatisfiable


let refine solution quals = function
    SubType(env, guard, f1, FVar(subs, k2)) ->
      let envp = environment_predicate solution env in
      let p1 = frame_predicate solution ("A", f1) in
        Prover.push (big_and [envp; guard; p1]);
        let qual_holds q =
          let qp = subst_quals_predicate "A" subs [q] in
            Prover.valid qp
        in
        let well_formed_quals = List.filter (qualifier_well_formed env) quals in
        let qset = QualifierSet.from_list (List.filter qual_holds well_formed_quals) in
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
      let unsat_constr =
        List.find (fun c -> not (constraint_sat solution c)) cs in
        Printf.printf "Solving %s\n\n" (pprint_constraint unsat_constr);
        solve_rec (refine solution quals unsat_constr)
    with Not_found ->
      solution
  in
  let qset = QualifierSet.from_list quals in
    Printf.printf "Constraints:\n\n";
    List.iter (fun c -> Printf.printf "%s\n\n" (pprint_constraint c)) constrs;
    solve_rec (Solution.create qset)
