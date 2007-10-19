open TheoremProver
open Format

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * Predicate.t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

type subrefinement_constraint =
    Frame.t Lightenv.t * Predicate.t * Frame.refinement * Frame.refinement

type well_formed_refinement_constraint =
    Frame.t Lightenv.t * Frame.refinement

type refinement_constraint =
  | SubRefinement of subrefinement_constraint
  | WFRefinement of well_formed_refinement_constraint

module VarName = struct
  type t = Path.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module VarMap = Map.Make(VarName)

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

let rec solve_wf_constraints solution = function
  | [] -> solution
  | (env, (subs, Frame.Qvar k)) :: cs ->
      let solution' = solve_wf_constraints solution cs in
      let refined_quals =
        List.filter
          (fun q -> Frame.refinement_well_formed env solution (subs, Frame.Qconst [q]))
          (try Lightenv.find k solution with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k); raise Not_found))
      in Lightenv.add k refined_quals solution'
  | _ :: cs -> solve_wf_constraints solution cs
      (* Nothing to do here; we can check satisfiability later *)

let refine solution = function
  | (env, guard, r1, (subs, Frame.Qvar k2)) ->
      let envp = environment_predicate solution env in
      let p1 = Frame.refinement_predicate solution qual_test_var r1 in
        Bstats.time "refinement query" Prover.push (Predicate.big_and [envp; guard; p1]);
        let qual_holds q =
          if Bstats.time "refinement query" Prover.valid
            (Frame.refinement_predicate solution qual_test_var (subs, Frame.Qconst [q])) then
            Some q
          else
            None
        in
        let refined_quals =
          Misc.map_filter qual_holds (try Lightenv.find k2 solution with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k2); raise Not_found)) in
          Bstats.time "refinement query" Prover.pop ();
          Lightenv.add k2 refined_quals solution
  | _ -> solution
      (* With anything else, there's nothing to refine, just to check later with
         check_satisfied *)

let env_refinement_vars env =
  let add_var _ f l =
    match Frame.refinement_var f with
      | None -> l
      | Some v -> v::l
  in Lightenv.fold add_var env []

let make_variable_constraint_map cstrs =
  let rec make_rec map = function
    | (env, _, (_, qe), (_, Frame.Qvar k)) as c :: cs ->
        let env_vars = env_refinement_vars env in
        let r1_vars = match qe with
          | Frame.Qvar k -> [k]
          | _ -> []
        in
        let add_cstr mp v =
          let new_cstrs = c :: (try VarMap.find v map with Not_found -> []) in
            VarMap.add v new_cstrs mp
        in 
        let map' = List.fold_left add_cstr map (List.flatten [env_vars; r1_vars]) in
        make_rec map' cs
    | _ :: cs -> make_rec map cs
    | [] -> map
  in make_rec VarMap.empty cstrs

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

let check_satisfied solution cstrs =
  try
    match List.find (fun c -> not (constraint_sat solution c)) cstrs with
      | SubRefinement (env, guard, r1, r2) ->
          (* If we have a literal RHS and we're unsatisfied, we're hosed -
             either the LHS is a literal and we can't do anything, or it's
             a var which has been pushed up by other constraints and, again,
             we can't do anything *)
          fprintf std_formatter
            "@[Unsatisfiable@ literal@ Subtype:@ (%a@ <:@ %a)@\nEnv:@ %a@\nSubref:%a@ ->@ %a@\n@]"
            Frame.pprint_refinement r1 Frame.pprint_refinement r2
            pprint_env_pred (solution, env) pprint_ref_pred (solution, r1) pprint_ref_pred (solution, r2);
          raise Unsatisfiable
      | WFRefinement _ ->
          fprintf std_formatter "@[Unsatisfiable@ literal@ WF@]";
          raise Unsatisfiable
  with Not_found -> ()

let rec divide_constraints_by_form wf refi = function
  | [] -> (wf, refi)
  | SubRefinement r :: cs ->
      divide_constraints_by_form wf (r :: refi) cs
  | WFRefinement w :: cs ->
      divide_constraints_by_form (w :: wf) refi cs

let solve_constraints quals constrs =
  if !Clflags.dump_constraints then
    Oprint.print_list pprint (fun ppf -> fprintf ppf "@.@.")
      std_formatter constrs;
  let cs = split constrs in
  let (wfs, refis) = divide_constraints_by_form [] [] cs in
  let init_solution = initial_solution cs quals in
  let solution' = solve_wf_constraints init_solution wfs in
  let cstr_map = make_variable_constraint_map refis in
  let rec solve_rec sol = function
    | [] -> sol
    | sr :: wklist ->
        let sol' = refine sol sr in
          if not (Lightenv.equal (=) sol' sol) then
            let wklist' =
              match sr with
                | (_, _, _, (_, Frame.Qvar k)) ->
                    (try VarMap.find k cstr_map with Not_found -> []) @ wklist
                | _ -> wklist
            in solve_rec sol' wklist'
          else solve_rec sol' wklist
  in
  let solution = Bstats.time "refining" (solve_rec solution') refis in
    Bstats.time "testing solution" (check_satisfied solution) cs;
    solution
