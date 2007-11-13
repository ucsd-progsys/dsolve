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

module Solution = Hashtbl.Make(VarName)

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
              let invar r a b =
                SubFrame(env, guard, a, b)
                :: SubFrame(env, guard, b, a)
                :: r
              in
                (* pmr: because we were only filtering through invariant types anyway, we might
                   as well just use invariants until we start getting problems from it ---
                   for now, it's too much trouble to work around all the BigArray stuff *)
                split_rec (SubRefinement(env, guard, r1, r2)::flat) (List.fold_left2 invar cs l1 l2)
              (*let subt a b = SubFrame(env, guard, a, b) in*)
              (*if Path.same p1 Predef.path_array
                 || Path.same p1 (Builtins.ext_find_type_path "ref") then
							    split_rec (SubRefinement(env, guard, r1, r2)::flat) (List.append (List.map2 invar l1 l2) cs)
              (*else if Path.same p1 Predef.path_int then (* must find path for tuple oh no i hope it's a path *)
                  split_rec (SubRefinement(env, guard, r1, r2)::flat) (List.append (List.map2 subt l1 l2) cs)*)
              else
                  (Printf.printf "Unsupported type into split: %s\n" (Path.name p1); assert false)*)
          | (Frame.Ftuple t1s, Frame.Ftuple t2s) ->
              let make_subframe t1 t2 = SubFrame(env, guard, t1, t2) in
                split_rec flat ((List.map2 make_subframe t1s t2s) @ cs)
          | (Frame.Frecord (_, recframes1, r1), Frame.Frecord (_, recframes2, r2)) ->
              let make_subframe rest (recf1, _, muta) (recf2, _, _) =
                let invar_cs = match muta with
                  | Asttypes.Immutable -> rest
                  | Asttypes.Mutable -> SubFrame (env, guard, recf2, recf1) :: rest
                in SubFrame (env, guard, recf1, recf2) :: invar_cs
              in
              let new_cs = List.fold_left2 make_subframe cs recframes1 recframes2 in
                (* ming: i'm not sure if i believe the following *)
              let new_flat =
                if List.exists (fun (_, _, muta) -> muta = Asttypes.Mutable) recframes1 then
                  SubRefinement (env, guard, r2, r1) :: flat
                else flat
              in split_rec (SubRefinement (env, guard, r1, r2) :: new_flat) new_cs
          | (f1, f2) -> printf "@[Can't@ split:@ %a@ <:@ %a@]" Frame.pprint f1 Frame.pprint f2; assert false
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
              (* pmr: this case should go in favor of the general one below *)
              (* We add the test variable to the environment with the current frame;
                 this makes type checking easier *)
              split_rec (WFRefinement (Lightenv.add qual_test_var f env, r) :: flat) cs
          | Frame.Ftuple ts ->
              split_rec flat ((List.map (fun t -> WFFrame (env, t)) ts) @ cs)
          | Frame.Frecord (_, fs, r) ->
              let wf_rec rest (recf, _, _) = WFFrame (env, recf) :: rest in
              let new_cs = List.fold_left wf_rec cs fs in
                split_rec (WFRefinement (Lightenv.add qual_test_var f env, r) :: flat) new_cs
          | Frame.Fvar _
          | Frame.Funknown ->
              split_rec flat cs
	        | Frame.Fconstr (_, l, r) ->
	            split_rec (WFRefinement(Lightenv.add qual_test_var f env, r)::flat) 
		          (List.append (List.map (function li -> WFFrame(env, li)) l) cs)
          (*| _ -> assert false*)
        end
  in split_rec [] cstrs

let solution_map solution = Solution.find solution

let environment_predicate solution env =
  Predicate.big_and (Lightenv.maplist (Frame.predicate (solution_map solution)) env)

let constraint_sat solution = function
  | SubRefinement (env, guard, r1, r2) ->
      let envp = environment_predicate solution env in
      let smap = solution_map solution in
      let p1 = Frame.refinement_predicate smap qual_test_var r1 in
      let p2 = Frame.refinement_predicate smap qual_test_var r2 in
        TheoremProver.backup_implies (Predicate.big_and [envp; guard; p1]) p2
  | WFRefinement (env, r) ->
    (* ming: this is an error check. it shouldn't be possible for this to be tripped*)
      Frame.refinement_well_formed env (solution_map solution) r qual_test_var

let pprint_env_pred ppf (solution, env) = 
  Predicate.pprint ppf (environment_predicate solution env)

let pprint_subref solution ppf (env, guard, r1, r2) =
  fprintf ppf "@[Env:@ %a;@;<1 2>Guard:@ %a@;<1 0>|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
    pprint_env_pred (solution, env)
    Predicate.pprint guard Frame.pprint_refinement r1 Frame.pprint_refinement r2

let pprint_ref_pred ppf (solution, r) =
  Predicate.pprint ppf (Frame.refinement_predicate (solution_map solution) qual_test_var r)

exception Unsatisfiable of Qualifier.t list Solution.t

let rec solve_wf_constraints solution = function
  | [] -> ()
  | (env, (subs, Frame.Qvar k)) :: cs ->
      (* ming: we have to pass qual_test_var into the WF checker now because
               we've agreed with the splitting code to use that as the predicate
               var. the only way around this would be to keep the frame from
               the WFFrame up until here, the solver, essentially obviating
               splitting. *)
      let refined_quals =
        List.filter
          (fun q -> Frame.refinement_well_formed env (solution_map solution) (subs, Frame.Qconst [q]) qual_test_var)
          (try Solution.find solution k with Not_found -> (Printf.printf "Couldn't find: %s" (Path.name k); raise Not_found))
      in Solution.replace solution k refined_quals;
        solve_wf_constraints solution cs
  | _ :: cs -> solve_wf_constraints solution cs
      (* Nothing to do here; we can check satisfiability later *)

(* ming: we should aggregate these tracking and statistics vals *)
let num_refines = ref 0

let solved_constraints = ref 0
let valid_constraints = ref 0

type refinement_change =
  | Solution_unchanged
  | Solution_changed

(* Refine a constraint with variables on both sides and no substitutions *)
let refine_simple solution k1 k2 =
  let k1_quals = Solution.find solution k1 in
  let result = ref Solution_unchanged in
  let filter_qual q =
    if List.mem q k1_quals then true else (result := Solution_changed; false)
  in
  let refined_quals = List.filter filter_qual (Solution.find solution k2) in
    Solution.replace solution k2 refined_quals;
    !result

let refine solution = function
  | (_, _, ([], Frame.Qvar k1), ([], Frame.Qvar k2))
      when not (!Clflags.no_simple || !Clflags.verify_simple) ->
      refine_simple solution k1 k2
  | (env, guard, r1, (rhs_subs, Frame.Qvar k2)) as cstr ->
      let _ = num_refines := !num_refines + 1 in
      let make_lhs (env, guard, r1, _) =
        let envp = environment_predicate solution env in
        let p1 = Frame.refinement_predicate (solution_map solution) qual_test_var r1 in
          Predicate.big_and [envp; guard; p1]
      in
      let lhs_preds = Frame.refinement_conjuncts (solution_map solution) qual_test_var r1 in
      let result = ref Solution_unchanged in
      let qual_holds q =
        let rhs = Frame.refinement_predicate (solution_map solution) qual_test_var (rhs_subs, Frame.Qconst [q]) in
        let res =
          let lhs = make_lhs cstr in
          let pres = Bstats.time "refinement query" (TheoremProver.implies lhs) rhs in
            incr solved_constraints;
            if pres then incr valid_constraints;
            pres
        in
        let resopt = (not !Clflags.no_simple_subs && List.mem rhs lhs_preds) || res
        in
          if res != resopt then begin
            (* Format.printf "@[Disagree on query (prover: %B, prop: %B) %a;@;<1 0>%a |- %a <:@;<1 2>%a@]@.@." res resopt
              pprint_env_pred (solution, env) Predicate.pprint guard
              Frame.pprint_refinement (lhs_subs, Frame.Qconst lhs_quals) Frame.pprint_refinement (rhs_subs, Frame.Qconst [q]); *)
            assert false
          end
          else begin
            if not res then result := Solution_changed;
            res
          end
      in
      let refined_quals = List.filter qual_holds (Solution.find solution k2) in
        Solution.replace solution k2 refined_quals;
        !result
  | _ -> Solution_unchanged
      (* With anything else, there's nothing to refine, just to check later with
         check_satisfied *)

let env_refinement_vars env =
  let add_vars _ f l = Frame.refinement_vars f @ l in
    Lightenv.fold add_vars env []

let lhs_vars (env, _, (_, qe), _) =
  let env_vars = env_refinement_vars env in
    match qe with
      | Frame.Qvar k -> k::env_vars
      | _ -> env_vars

let make_variable_constraint_map cstrs =
  let rec make_rec map = function
    | (_, _, _, (_, Frame.Qvar k)) as c :: cs ->
        let add_cstr m v =
          let new_cstrs = c :: (try VarMap.find v m with Not_found -> []) in
            VarMap.add v new_cstrs m
        in
        let map = List.fold_left add_cstr map (lhs_vars c) in make_rec map cs
    | _ :: cs -> make_rec map cs
    | [] -> map
  in make_rec VarMap.empty cstrs

(* Form the initial solution by mapping all constrained variables to all
   qualifiers.  This was somewhat easier than adding any notion of "default"
   to Lightenv. *)
let initial_solution cstrs quals =
  let sol = Solution.create 17 in
  let add_refinement_var = function
    | (_, Frame.Qconst _) -> ()
    | (_, Frame.Qvar k) -> Solution.replace sol k quals
  in
  let add_constraint_vars = function
    | SubRefinement (_, _, r1, r2) -> List.iter add_refinement_var [r1; r2]
    | WFRefinement (_, r) -> add_refinement_var r
  in
    List.iter add_constraint_vars cstrs;
    sol

let check_satisfied solution cstrs =
  try
    match List.find (fun c -> not (constraint_sat solution c)) cstrs with
      | SubRefinement (env, guard, r1, r2) ->
          (* If we have a literal RHS and we're unsatisfied, we're hosed -
             either the LHS is a literal and we can't do anything, or it's
             a var which has been pushed up by other constraints and, again,
             we can't do anything *)
          fprintf std_formatter
            "@.@.@[Unsatisfiable@ literal@ Subtype:@ (%a@ <:@ %a)@\nEnv:@ %a@\nGuard:@ %a@\nSubref:%a@ ->@ %a@\n@]"
            Frame.pprint_refinement r1 Frame.pprint_refinement r2
            pprint_env_pred (solution, env) Predicate.pprint guard pprint_ref_pred (solution, r1) pprint_ref_pred (solution, r2);
          raise (Unsatisfiable solution)
      | WFRefinement (env, f) ->
          fprintf std_formatter "@[Unsatisfied@ WF:@ %a@\nEnv:@ %a@\nWF:@ %a@]" 
            Frame.pprint_refinement f
            (Lightenv.pprint Frame.pprint) env
            pprint_ref_pred (solution, f);
          raise (Unsatisfiable solution)
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
  Solution.iter add solution; QualifierSet.cardinal !quals 

let count_variables solution =
  let sum = ref 0 in
  let add k ql = sum := 1 + !sum in
  Solution.iter add solution; !sum

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
  Solution.iter add solution; (!sum, !max, !min)

module Constraint = struct
  type t = subrefinement_constraint
  let compare (env1, _, _, _) (env2, _, _, _) = -(Lightenv.compare env1 env2)
    (* We want the smallest environment to be the maximum because our worklist
       is a priority queue *)
end

exception Empty_worklist

module Worklist = struct
  module WHeap = Heap.Imperative(Constraint)

  type t =
    | Workheap of WHeap.t
    | Worklist of subrefinement_constraint list

  let empty () = if !Clflags.use_list then Worklist [] else Workheap (WHeap.create 17)

  let pop = function
    | Worklist [] -> raise Empty_worklist
    | Worklist (c :: cs) -> (c, Worklist cs)
    | (Workheap h) as wh ->
        try
          let max = WHeap.maximum h in
            WHeap.remove h;
            (max, wh)
        with Heap.EmptyHeap -> raise Empty_worklist

  let push cs = function
    | Worklist l -> Worklist (l @ cs)
    | (Workheap h) as wh ->
        List.iter (fun c -> WHeap.add h c) cs;
        wh
end

let solve_constraints quals constrs =
  if !Clflags.dump_constraints then
    Oprint.print_list pprint (fun ppf -> fprintf ppf "@.@.")
      std_formatter constrs;
  let cs = split constrs in
  let (wfs, refis) = divide_constraints_by_form [] [] cs in
  let inst_quals = List.length quals in
  let _ = Printf.printf "%d instantiated qualifiers\n\n" inst_quals in

  let solution = initial_solution cs quals in

  let num_vars = count_variables solution in
  let _ = Printf.printf "%d variables\n\n" num_vars in 
  let _ = Printf.printf "%d total quals\n\n" (num_vars * inst_quals) in

  Bstats.time "solving wfs" (solve_wf_constraints solution) wfs;

  Printf.printf "%d unique qualifiers after solving wf\n\n" (count_qualifiers solution);
  let (sum, max, min) = count_total_qualifiers solution in
  Printf.printf "Quals:\n\tTotal: %d\n\tAvg: %f\n\tMax: %d\n\tMin: %d\n\n"
                sum ((float_of_int sum) /. (float_of_int num_vars)) max min;
  Printf.printf "%d split subtyping constraints\n\n" (List.length refis);
  Printf.printf "%d simple subtyping constraints\n\n"
    (List.length (List.filter is_simple_constraint refis));
  print_flush ();
  let cstr_map = make_variable_constraint_map refis in
  let rec solve_rec wklist =
    let next = try Some (Worklist.pop wklist) with Empty_worklist -> None in
      match next with
        | Some (cstr, rest) ->
            begin match cstr with
              | (_, _, _, (_, Frame.Qvar k)) ->
                  (* pmr: removed opt-
                     Find all the constraints with RHSes identical to this one; they can be solved
                     in one pass by or-ing all the LHSes together, saving a few prover calls *)
                  (* pmr: of course, as implemented below, this is quite inefficient - we shouldn't be
                     searching the worklist.  OTOH, we're trying to gain on time spent in the prover, so
                     we can worry about the efficiency of this later *)
                  let _ =
                    match refine solution cstr with
                      | Solution_changed ->
                          Worklist.push (try VarMap.find k cstr_map with Not_found -> []) rest
                      | Solution_unchanged -> rest
                  in solve_rec wklist
              | _ -> solve_rec rest
            end
        | None -> ()
  in

  (* Find the "roots" of the constraint graph - all those constraints that don't
     have a variable in the LHS AND also the vars we solved for well-formedness in the previous
     round (because we need to be sure we visit their children) *)
  (* pmr: this is sounding like basically everything, so we'll just throw everything on at the
     start to be sure; the heap should ensure we get to the roots first, though *)
    (*let roots = List.filter (fun c -> match lhs_vars c with [] -> true | _ -> false) refis in*)
  let init_wklist = Worklist.push refis (Worklist.empty ()) in

  Bstats.time "refining subtypes" solve_rec init_wklist;

  let _ = Printf.printf "solution refinement completed:\n\t%d iterations of refine\n\n" !num_refines in
  let _ = Format.printf "@[Solved@ %d@ constraints;@ %d@ valid@]@.@." !solved_constraints !valid_constraints in
  let _ = TheoremProver.dump_simple_stats () in
  let _ = TheoremProver.clear_cache () in
  let _ = flush stdout in

  if !Clflags.dump_constraints then
    Oprint.print_list (pprint_subref solution) (fun ppf -> fprintf ppf "@.@.")
      std_formatter refis;
  Bstats.time "testing solution" (check_satisfied solution) cs;
  solution
