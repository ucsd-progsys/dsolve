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

(* Unique variable to qualify when testing sat, applicability of qualifiers, etc. *)
let qual_test_var = Path.mk_ident "AA"

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
    (* ming: for type checking, when we split WFFrames now, we need to keep
             the frame around. the paper method for doing this is to insert
             the frame into the environment, so that's what we do here.
             note that we've just agreed on using the qual_test_var ident
             for predicate vars (it's passed into the solver later...) *)
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
              (* We add the test variable to the environment with the current frame;
                 this makes type checking easier *)
              split_rec (WFRefinement (Lightenv.add qual_test_var f env, r) :: flat) cs
          | Frame.Ftuple (t1, t2) ->
              split_rec flat (List.append [WFFrame(env, t1); WFFrame(env, t2)] cs)
          | Frame.Fvar _
          | Frame.Funknown ->
              split_rec flat cs
	        | Frame.Fconstr (_, l, r) ->
	            split_rec (WFRefinement(Lightenv.add qual_test_var f env, r)::flat) 
		          (List.append (List.map (function li -> WFFrame(env, li)) l) cs)
          (*| _ -> assert false*)
        end
  in split_rec [] cstrs

let environment_predicate solution env =
  Predicate.big_and (Lightenv.maplist (Frame.predicate solution) env)

let constraint_sat solution = function
  | SubRefinement (env, guard, r1, r2) ->
      let envp = environment_predicate solution env in
      let p1 = Frame.refinement_predicate solution qual_test_var r1 in
      let p2 = Frame.refinement_predicate solution qual_test_var r2 in
        TheoremProver.implies (Predicate.big_and [envp; guard; p1]) p2
  | WFRefinement (env, r) ->
    (* ming: this is an error check. it shouldn't be possible for this to be tripped*)
      Frame.refinement_well_formed env solution r qual_test_var

let pprint_env_pred ppf (solution, env) = 
  Predicate.pprint ppf (environment_predicate solution env)

let pprint_ref_pred ppf (solution, r) =
  Predicate.pprint ppf (Frame.refinement_predicate solution qual_test_var r)

exception Unsatisfiable

let rec solve_wf_constraints solution = function
  | [] -> solution
  | (env, (subs, Frame.Qvar k)) :: cs ->
      let solution' = solve_wf_constraints solution cs in
      (* ming: we have to pass qual_test_var into the WF checker now because
               we've agreed with the splitting code to use that as the predicate
               var. the only way around this would be to keep the frame from
               the WFFrame up until here, the solver, essentially obviating
               splitting. *)
      let refined_quals =
        List.filter
          (fun q -> Frame.refinement_well_formed env solution' (subs, Frame.Qconst [q]) qual_test_var)
          (try Lightenv.find k solution' with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k); raise Not_found))
      in Lightenv.add k refined_quals solution'
  | _ :: cs -> solve_wf_constraints solution cs
      (* Nothing to do here; we can check satisfiability later *)

(* ming: we should aggregate these tracking and statistics vals *)
let num_refines = ref 0

let compare_constraints (env1, _, _, _) (env2, _, _, _) = Lightenv.compare env1 env2

let solved_constraints = ref 0
let valid_constraints = ref 0

let refine solution = function
  | (_, _, ([], Frame.Qvar k1), ([], Frame.Qvar k2)) ->
      let k1_quals = Lightenv.find k1 solution in
      let refined_quals = List.filter (fun q -> List.mem q k1_quals) (Lightenv.find k2 solution) in
        Lightenv.add k2 refined_quals solution
  | (_, _, _, (subs, Frame.Qvar k2)) as r ->
      let _ = num_refines := !num_refines + 1 in
      let make_lhs (env, guard, r1, _) =
        let envp = environment_predicate solution env in
        let p1 = Frame.refinement_predicate solution qual_test_var r1 in
          (Predicate.big_and [envp; guard; p1])
      in
        let lhs = make_lhs r in
        let qual_holds q =
          let rhs = (Frame.refinement_predicate solution qual_test_var (subs, Frame.Qconst [q])) in
            begin try
              let res = Bstats.time "refinement query" TheoremProver.implies lhs rhs in
                incr solved_constraints;
                if res then incr valid_constraints;
                Printf.printf "Solved %d constraints; %d valid\n\n" !solved_constraints !valid_constraints;
                print_flush ();
                if !Clflags.dump_queries then
                  Format.fprintf std_formatter "@[%a@ =>@ %a@ (%s)@]@.@."
                    Predicate.pprint lhs Predicate.pprint rhs (if res then "SAT" else "UNSAT");
                res
            with TheoremProver.Provers_disagree (default, backup) ->
              begin
                fprintf std_formatter
                  "@[Theorem@ prover@ insanity:@]@.";
                fprintf std_formatter
                  "@[Query:@;<1 2>%a@;<1 2>=>@;<1 2>%a@]@."
                  Predicate.pprint lhs Predicate.pprint rhs;
                fprintf std_formatter
                  "@[Default@ prover:@ %B,@ backup@ prover:@ %B@]@."
                  default backup;
                assert false
              end
            end
        in
        let refined_quals =
          List.filter qual_holds (try Lightenv.find k2 solution with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k2); raise Not_found))
        in Lightenv.add k2 refined_quals solution
  | _ -> solution
      (* With anything else, there's nothing to refine, just to check later with
         check_satisfied *)

let env_refinement_vars env =
  let add_var _ f l =
    match Frame.refinement_var f with
      | None -> l
      | Some v -> v::l
  in Lightenv.fold add_var env []

let lhs_vars (env, _, (_, qe), _) =
  let env_vars = env_refinement_vars env in
    match qe with
      | Frame.Qvar k -> k::env_vars
      | _ -> env_vars

let make_variable_constraint_map cstrs =
  let rec make_rec map = function
    | (_, _, (_, _), (_, Frame.Qvar k)) as c :: cs ->
        let add_cstr mp v =
          let new_cstrs = c :: (try VarMap.find v map with Not_found -> []) in
            VarMap.add v new_cstrs mp
        in 
        let map' = List.fold_left add_cstr map (lhs_vars c) in make_rec map' cs
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
            "@.@.@[Unsatisfiable@ literal@ Subtype:@ (%a@ <:@ %a)@\nEnv:@ %a@\nSubref:%a@ ->@ %a@\n@]"
            Frame.pprint_refinement r1 Frame.pprint_refinement r2
            pprint_env_pred (solution, env) pprint_ref_pred (solution, r1) pprint_ref_pred (solution, r2);
          raise Unsatisfiable
      | WFRefinement (env, f) ->
          fprintf std_formatter "@[Unsatisfied@ WF:@ %a@\nEnv:@ %a@\nWF:@ %a@]" 
            Frame.pprint_refinement f
            (Lightenv.pprint Frame.pprint) env
            pprint_ref_pred (solution, f);
          raise Unsatisfiable
  with Not_found -> ()

let rec divide_constraints_by_form wf refi = function
  | [] -> (wf, refi)
  | SubRefinement r :: cs ->
      divide_constraints_by_form wf (r :: refi) cs
  | WFRefinement w :: cs ->
      divide_constraints_by_form (w :: wf) refi cs

let is_simple_constraint = function
  | (_, _, ([], _), ([], _)) -> true
  | _ -> false

module QualifierSet = Set.Make(Qualifier)

let count_qualifiers solution =
  let quals = ref QualifierSet.empty in
  let add k ql = List.iter (fun q -> quals := QualifierSet.add q !quals) ql in
  Lightenv.iter add solution; QualifierSet.cardinal !quals 

let count_variables solution =
  let sum = ref 0 in
  let add k ql = sum := 1 + !sum in
  Lightenv.iter add solution; !sum

let count_total_qualifiers solution = 
  let sum = ref 0 in
  let max = ref 0 in
  let min = ref 0 in
  let add k ql = 
    let l = List.length ql in
    sum := l + !sum;
    max := if l > !max then l else !max;
    min := if !min = 0 then l else
              if l < !min then l else !min in
  Lightenv.iter add solution; (!sum, !max, !min)

let solve_constraints quals constrs =
  if !Clflags.dump_constraints then
    Oprint.print_list pprint (fun ppf -> fprintf ppf "@.@.")
      std_formatter constrs;
  let cs = split constrs in
  let (wfs, refis) = divide_constraints_by_form [] [] cs in
  
  let refis = List.fast_sort compare_constraints refis in
  let inst_quals = List.length quals in
  let _ = Printf.printf "%d instantiated qualifiers\n\n" inst_quals in

  let init_solution = initial_solution cs quals in

  let num_vars = count_variables init_solution in
  let _ = Printf.printf "%d variables\n\n" num_vars in 
  let _ = Printf.printf "%d total quals\n\n" (num_vars * inst_quals) in

  let solution' = Bstats.time "solving wfs" (solve_wf_constraints init_solution) wfs in

  Printf.printf "%d unique qualifiers after solving wf\n\n" (count_qualifiers solution');
  let (sum, max, min) = count_total_qualifiers solution' in
  Printf.printf "Quals:\n\tTotal: %d\n\tAvg: %f\n\tMax: %d\n\tMin: %d\n\n"
                sum ((float_of_int sum) /. (float_of_int num_vars)) max min;
  Printf.printf "%d split subtyping constraints\n\n" (List.length refis);
  Printf.printf "%d simple subtyping constraints\n\n"
    (List.length (List.filter is_simple_constraint refis));
  print_flush ();
  let cstr_map = make_variable_constraint_map refis in
  let rec solve_rec sol = function
    | [] -> sol
    | (_, _, _, (_, Frame.Qvar k)) as r :: rest ->
        (* pmr: removed opt-
           Find all the constraints with RHSes identical to this one; they can be solved
           in one pass by or-ing all the LHSes together, saving a few prover calls *)
        (* pmr: of course, as implemented below, this is quite inefficient - we shouldn't be
           searching the worklist.  OTOH, we're trying to gain on time spent in the prover, so
           we can worry about the efficiency of this later *)
        Printf.printf "Worklist has size %d\n\n" (List.length rest);
        print_flush ();
        let sol' = refine sol r in
        let wklist' =
          if not (Lightenv.equal (=) sol' sol) then
            rest @ (try VarMap.find k cstr_map with Not_found -> [])
          else rest
        in solve_rec sol' wklist'
    | _ :: wklist -> solve_rec sol wklist
  in
  (* Find the "roots" of the constraint graph - all those constraints that don't
     have a variable in the LHS *)
  let init_wklist = refis in (* List.filter (fun c -> match lhs_vars c with [] -> true | _ -> false) refis in *)
  Printf.printf "%d constraint graph roots\n\n" (List.length init_wklist);
  let solution = Bstats.time "refining subtypes" (solve_rec solution') init_wklist in

  let _ = Printf.printf "solution refinement completed:\n\t%d iterations of refine\n\n" !num_refines in
  let _ = flush stdout in

    Bstats.time "testing solution" (check_satisfied solution) cs;
    solution
