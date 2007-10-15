open TheoremProver
open Format

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

type refinement_constraint =
  | SubRefinement of Frame.t Lightenv.t * Predicate.t * Frame.refinement * Frame.refinement
  | WFRefinement of Frame.t Lightenv.t * Frame.refinement

let pprint ppf = function
  | SubFrame (_, _, f1, f2) ->
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" Frame.pprint f1 Frame.pprint f2
  | WFFrame (_, f) ->
      Frame.pprint ppf f

let environment = function
  | SubFrame (env, _, _, _) -> env
  | WFFrame (env, _) -> env

let split cstrs =
  let rec split_rec flat = function
    | [] -> flat
    | SubFrame(env, guard, f1, f2) :: cs ->
        begin match (f1, f2) with
          | (Frame.Farrow (l1, f1, f1'), Frame.Farrow (l2, f2, f2')) ->
              (* If the labels disagree, we don't attempt to resolve it.
                 Instead, we just proceed with the best information we have,
                 probably losing chances to assert qualifiers along the way. *)
              let env' = match (l1, l2) with
                | (Some x, None)
                | (None, Some x) ->
                    Lightenv.add x f2 env
                | (Some x, Some y) when Path.same x y ->
                    Lightenv.add x f2 env
                | _ -> env
              in split_rec flat
                   (SubFrame (env, guard, f2, f1)
                    :: SubFrame (env', guard, f1', f2')
                    :: cs)
          | (Frame.Fconstr (p1, [], r1), Frame.Fconstr (p2, [], r2)) ->
              split_rec (SubRefinement (env, guard, r1, r2) :: flat) cs
          | (Frame.Fvar _, Frame.Fvar _)
          | (Frame.Funknown, Frame.Funknown) ->
              split_rec flat cs
					| (Frame.Fconstr (p1, l1, r1), Frame.Fconstr(p2, l2, r2)) ->
              let invar a b = [SubFrame(env, guard, a, b); SubFrame(env, guard, b, a)] in
              (*let subt a b = SubFrame(env, guard, a, b) in*)
              if Path.same p1 Predef.path_array 
                 || Path.same p1 (Builtins.ext_find_type_path "ref") then 
							    split_rec (SubRefinement(env, guard, r1, r2)::flat) (List.append (invar (List.hd l1) (List.hd l2)) cs)
              (*else if Path.same p1 Predef.path_int then (* must find path for tuple oh no i hope it's a path *)
                  split_rec (SubRefinement(env, guard, r1, r2)::flat) (List.append (List.map2 subt l1 l2) cs)*)
              else
                  (Printf.printf "Unsupported type into split: %s\n" (Path.name p1); assert false)
          | (Frame.Ftuple(t1, t2), Frame.Ftuple(t1', t2')) ->
              split_rec flat (List.append [SubFrame(env, guard, t1, t1'); SubFrame(env, guard, t2, t2')] cs) 
          | _ -> assert false
        end
    | WFFrame(env, f) :: cs ->
        begin match f with
          | Frame.Farrow (l, f, f') ->
              let env' = match l with
                | None -> env
                | Some x -> Lightenv.add x f env
              in split_rec flat
                   (WFFrame (env, f)
                    :: WFFrame (env', f')
                    :: cs)
          | Frame.Fconstr (_, [], r) ->
              split_rec (WFRefinement (env, r) :: flat) cs
          | Frame.Ftuple (t1, t2) ->
              split_rec flat (List.append [WFFrame(env, t1); WFFrame(env, t2)] cs)
          | Frame.Fvar _
          | Frame.Funknown ->
              split_rec flat cs
	  | Frame.Fconstr (_, l, r) ->
	      split_rec (WFRefinement(env, r)::flat) 
		(List.append (List.map (function li -> WFFrame(env, li)) l) cs)
          (*| _ -> assert false*)
        end
  in split_rec [] cstrs

let environment_predicate solution env =
  Predicate.big_and (Lightenv.maplist (Frame.predicate solution) env)

(* Unique variable to qualify when testing sat, applicability of qualifiers, etc. *)
let qual_test_var = Path.mk_ident "AA"

let constraint_sat solution = function
  | SubRefinement (env, guard, r1, r2) ->
      let envp = environment_predicate solution env in
      let p1 = Frame.refinement_predicate solution qual_test_var r1 in
      let p2 = Frame.refinement_predicate solution qual_test_var r2 in
        Prover.implies (Predicate.big_and [envp; guard; p1]) p2
  | WFRefinement (env, r) ->
      Frame.refinement_well_formed env solution r

let pprint_env_pred ppf (solution, env) = 
  Predicate.pprint ppf (environment_predicate solution env)

let pprint_ref_pred ppf (solution, r) =
  Predicate.pprint ppf (Frame.refinement_predicate solution qual_test_var r)

exception Unsatisfiable

let refine solution = function
  | WFRefinement (env, (subs, Frame.Qvar k)) ->
      let refined_quals =
        List.filter
          (fun q -> Frame.refinement_well_formed env solution (subs, Frame.Qconst [q]))
          (try Lightenv.find k solution with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k); raise Not_found))
      in Lightenv.add k refined_quals solution
  | SubRefinement (env, guard, r1, (subs, Frame.Qvar k2)) ->
      let envp = environment_predicate solution env in
      let p1 = Frame.refinement_predicate solution qual_test_var r1 in
        Prover.push (Predicate.big_and [envp; guard; p1]);
        let qual_holds q =
          if Prover.valid
            (Frame.refinement_predicate solution qual_test_var (subs, Frame.Qconst [q])) then
            Some q
          else
            None
        in
        let refined_quals =
          Misc.map_filter qual_holds (try Lightenv.find k2 solution with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k2); raise Not_found)) in
          Prover.pop();
          Lightenv.add k2 refined_quals solution
  | SubRefinement (env, guard, r1, r2) ->
      (* If we have a literal RHS and we're unsatisfied, we're hosed -
         either the LHS is a literal and we can't do anything, or it's
         a var which has been pushed up by other constraints and, again,
         we can't do anything *)
      fprintf std_formatter "@[Unsatisfiable@ literal@ Subtype:@ (%a@ <:@ %a)@ Env:@ %a@ Subref:%a@ ->@ %a@]" Frame.pprint_refinement r1 Frame.pprint_refinement r2 pprint_env_pred (solution, env) pprint_ref_pred (solution, r1) pprint_ref_pred (solution, r2);
      raise Unsatisfiable
  | WFRefinement _ ->
      fprintf std_formatter "@[Unsatisfiable@ literal@ WF@]";
      raise Unsatisfiable

(* Form the initial solution by mapping all constrained variables to all
   qualifiers.  This was somewhat easier than adding any notion of "default"
   to Lightenv. *)
let initial_solution cstrs quals =
  let add_refinement_var sol = function
    | (_, Frame.Qconst _) -> sol
    | (_, Frame.Qvar k) -> Lightenv.add k quals sol
  in
  let add_constraint_vars solution = function
    | SubRefinement (_, _, r1, r2) -> List.fold_left add_refinement_var solution [r1; r2]
    | WFRefinement (_, r) -> add_refinement_var solution r
  in List.fold_left add_constraint_vars Lightenv.empty cstrs

let solve_constraints quals constrs =
  if !Clflags.dump_constraints then
    Oprint.print_list pprint (fun ppf -> fprintf ppf "@\n@\n"; print_flush ())
      std_formatter constrs;
  let cs = split constrs in
  let rec solve_rec solution =
    try
      let unsat_constr = List.find (fun c -> not (constraint_sat solution c)) cs in
        solve_rec (refine solution unsat_constr)
    with Not_found -> solution
  in solve_rec (initial_solution cs quals)
