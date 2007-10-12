open Asttypes
open Typedtree
open Btype
open Types
open Constraint
open Longident
open Format

module LocationMap = Map.Make(struct type t = Location.t
                                     let compare = compare end)

let expression_to_pexpr e =
  match e.exp_desc with
    | Texp_constant (Const_int n) ->
	Predicate.PInt n
    | Texp_ident (id, _) ->
	Predicate.Var id
    | _ ->
        Predicate.Var (Path.mk_ident "dummy")

let constrain_expression tenv initenv exp initcstrs initframemap =
  let rec constrain e env guard cstrs framemap =
    let (f, cs, fm) =
      match (e.exp_desc, repr e.exp_type) with
	  (Texp_constant(Const_int n), {desc = Tconstr(path, [], _)}) ->
            (Frame.Fconstr (path, [], Builtins.equality_refinement (Predicate.PInt n)),
             cstrs, framemap)
  | (Texp_construct(cstrdesc, []), {desc = Tconstr(path, [], _)}) ->
            let cstrref =
              match cstrdesc.cstr_tag with
                  Cstr_constant n ->
                    Builtins.equality_refinement (Predicate.PInt n)
                | _ -> Frame.empty_refinement
            in (Frame.Fconstr (path, [], cstrref), cstrs, framemap)
	| (Texp_ifthenelse(e1, e2, Some e3), t) ->
            let f = Frame.fresh t in
            let (f1, cstrs1, fm1) = constrain e1 env guard cstrs framemap in
            let guardvar = Path.mk_ident "guard" in
            let true_tag =
              match (Env.lookup_constructor (Lident "true") tenv).cstr_tag with
                  Cstr_constant n -> n
                | _ -> assert false
            in
            let guardp = Predicate.equals (Predicate.Var guardvar, Predicate.PInt true_tag) in
            let env' = Lightenv.add guardvar f1 env in
            let guard2 = Predicate.And (guardp, guard) in
            let (f2, cstrs2, fm2) = constrain e2 env' guard2 cstrs1 fm1 in
            let guard3 = Predicate.And (Predicate.Not guardp, guard) in
            let (f3, cstrs3, fm3) = constrain e3 env' guard3 cstrs2 fm2 in
              (f,
               WFFrame(env, f)
               ::SubFrame(env', guard2, f2, f)
               ::SubFrame(env', guard3, f3, f)
               ::cstrs3,
               fm3)
	| (Texp_function([({pat_desc = Tpat_var x}, e')], _), t) ->
	    begin match Frame.fresh t with
              | Frame.Farrow (_, f, unlabelled_f') ->
                  let xp = Path.Pident x in
                  let env' = Lightenv.add xp f env in
                  let (f'', cstrs', fm') = constrain e' env' guard cstrs framemap in
                  (* Since the underlying type system doesn't have dependent
                     types, fresh can't give us the proper labels for the RHS.
                     Instead, we have to label it after the fact. *)
                  let f' = Frame.label_like unlabelled_f' f'' in
                  let f = Frame.Farrow (Some xp, f, f') in
                    (f,
                     WFFrame (env, f)
                     :: SubFrame (env', guard, f'', f')
                     :: cstrs',
                     fm')
              | _ -> assert false
	    end
	| (Texp_ident _, {desc = Tconstr (p, [], _)}) ->
            (Frame.Fconstr (p, [], Builtins.equality_refinement (expression_to_pexpr e)),
             cstrs, framemap)
  | (Texp_ident (id, _), t) ->
            (* pmr: Later, the env should probably take paths, not idents.
               Something to think about... *)
            let (f', ftemplate) = (Lightenv.find id env, Frame.fresh t) in
            (*let _ = fprintf std_formatter "@[instantiating@ %a@ %a@]" Frame.pprint f' Frame.pprint ftemplate in*)
            (*let _ = Frame.pprint Format.err_formatter f' in
            let _ = Frame.pprint Format.err_formatter ftemplate in *)
            let f = Frame.instantiate f' ftemplate in
              (f, cstrs, framemap)
	| (Texp_apply (e1, exps), _) ->
	    let constrain_application (f, cs, fm) = function
              | (Some e2, _) ->
	          begin match f with
		    | Frame.Farrow (l, f, f') ->
		        let (f2, cs', fm') = constrain e2 env guard cs fm in
		        let f'' = match l with
                          | Some x -> Frame.apply_substitution (x, expression_to_pexpr e2) f'
                              (* pmr: The soundness of this next line is suspect,
                                 must investigate (i.e., what if there's a var that might
                                 somehow be substituted that isn't because of this?  How
                                 does it interact w/ the None label rules for subtyping? *)
                          | None -> f'
                        in (f'', SubFrame (env, guard, f2, f) :: cs', fm')
		    | _ -> assert false
                  end
              | _ -> assert false
	    in List.fold_left constrain_application
                 (constrain e1 env guard cstrs framemap) exps
	| (Texp_let (Nonrecursive, [({pat_desc = Tpat_var x}, e1)], e2), t) ->
            let (f1, cstrs'', fm'') = constrain e1 env guard cstrs framemap in
            let env' = Lightenv.add (Path.Pident x) f1 env in
            let (f2, cstrs', fm') = constrain e2 env' guard cstrs'' fm'' in
            let f = Frame.fresh_with_labels t f2 in
              (f, SubFrame (env', guard, f2, f) :: cstrs', fm')
	| (Texp_let (Recursive, [({pat_desc = Tpat_var f}, e1)], e2), t) ->
            (* pmr: This is horrendous, but about the best we can do
               without using destructive updates.  We need to label
               the function we're binding in the environment where we
               also constrain the function.  Unfortunately, we don't
               have that label until after we constrain it!  So we do
               a first pass just to get the labels, then do a second
               with the proper labels added. *)
            let unlabelled_f1 = Frame.fresh e1.exp_type in
            let fp = Path.Pident f in
            let unlabelled_env = Lightenv.add fp unlabelled_f1 env in
            let (labelled_f1, _, _) = constrain e1 unlabelled_env guard cstrs framemap in
            let f1' = Frame.label_like unlabelled_f1 labelled_f1 in
            let env'' = Lightenv.add fp f1' env in
            let (f1, cstrs'', fm'') = constrain e1 env'' guard cstrs framemap in
            let env' = Lightenv.add fp f1 env in
            let (f2, cstrs', fm') = constrain e2 env' guard cstrs'' fm'' in
            let f = Frame.fresh_with_labels t f2 in
              (f,
               SubFrame (env', guard, f2, f)
               :: SubFrame (env'', guard, f1, f1')
               :: cstrs',
               fm')
	| (Texp_array(es), t) ->
						let f = Frame.fresh t in
						let (f, fs) = (function Frame.Fconstr(p, l, _) -> (Frame.Fconstr(p, l, Builtins.size_lit_refinement(List.length es)), l) | _ -> assert false) f in
						let list_rec (fs, c, m) e = (function (f, c, m) -> (f::fs, c, m)) (constrain e env guard c m) in
						let (fs', c, m) = List.fold_left list_rec ([], cstrs, framemap) es in
						let mksub b a = SubFrame(env, guard, a, b) in
						let c = List.append (List.map (mksub (List.hd fs)) fs') c in
						(f, WFFrame(env, f)::c, m)	
  | (Texp_sequence(e1, e2), t) ->
            let (f1, c, m) = constrain e1 env guard cstrs framemap in
            let (f2, c, m) = constrain e2 env guard c m in
            (f2, (SubFrame(env, guard, f1, Builtins.mk_unit ())::c), m)
	| (_, t) ->
(*
					(*let _ = !Oprint.out_type Format.std_formatter (Printtyp.tree_of_type_scheme t) in*)
					(*let _ = flush_all () in*)
          let _ = Printf.printf "%s" (Frame.type_structure t) in
					assert false
					(*let _ = Printf.printf "\n" in*)
					(*(Frame.fresh e.exp_type, cstrs, framemap)*)			 
*)
            (* pmr: need to print the offending expression, perhaps *)
            fprintf err_formatter "@[Warning:@ Don't@ know@ how@ to@ constrain@ expression,@;<1 0>defaulting@ to@ true@]@.\t Ty_structure: %s\n" (Frame.type_structure t);
            (Frame.fresh_without_vars t, cstrs, framemap)
    in (f, cs, LocationMap.add e.exp_loc f fm)
  in constrain exp initenv Predicate.True initcstrs initframemap

(* pmr: note we're operating in the environment created by typing the
   structure - it's entirely possible this has some bad corner cases *)
let constrain_structure tenv fenv initquals str =
  let rec constrain_rec quals cstrs fmap = function
    | [] -> (quals, cstrs, fmap)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs', fmap') = constrain_expression tenv fenv exp cstrs fmap in
          constrain_rec quals cstrs' fmap' srem
    | (Tstr_qualifier (name, (valu, pred))) :: srem ->
        let newquals = (Path.Pident name, Path.Pident valu, pred) :: quals in
          constrain_rec newquals cstrs fmap srem
		| (Tstr_value(_, _))::srem ->
				Printf.printf "Tstr_val unsupported."; assert false
    | _ -> assert false
  in constrain_rec initquals [] LocationMap.empty str

module QualifierSet = Set.Make(Qualifier)

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from the environment. *)
let instantiate_in_environments cstrs quals =
  let envs = List.map environment cstrs in
  let instantiate_qual qualset q =
    let instantiate_in_env qset env =
      try
        QualifierSet.add (Qualifier.instantiate env q) qset
      with Qualifier.Refinement_not_closed -> qset
    in List.fold_left instantiate_in_env qualset envs
  in QualifierSet.elements (List.fold_left instantiate_qual QualifierSet.empty quals)

let qualify_structure tenv fenv quals str =
  let (newquals, cstrs, fmap) = constrain_structure tenv fenv quals str in
  let instantiated_quals = instantiate_in_environments cstrs quals in
  let solution = solve_constraints instantiated_quals cstrs in
    (newquals, LocationMap.map (Frame.apply_solution solution) fmap)
