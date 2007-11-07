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

let under_lambda = ref 0

let constrain_expression tenv initenv exp initcstrs initframemap =
  let rec constrain e env guard cstrs framemap =
    let (f, cs, fm) =
      match (e.exp_desc, repr e.exp_type) with
	  (Texp_constant(Const_int n), {desc = Tconstr(path, [], _)}) ->
            let _ = Qualgen.add_constant n in
            (Frame.Fconstr (path, [], Builtins.equality_refinement (Predicate.PInt n)),
             cstrs, framemap)
  | (Texp_constant(Const_float n), t) ->
            (Frame.fresh t, cstrs, framemap)
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
                  let _ =
                    match t with 
                      {desc = Tarrow(_, t_in, t_out, _)} ->
                        Qualgen.add_label(xp, t_in)
                      | _ -> assert false
                  in
                    (*match e'.exp_type with  
                      {desc = Tarrow(_, t_in, _, _)} -> Printf.printf "%s%s\n" (Ident.name x) (Frame.type_structure t_in); Qualgen.add_label (xp, t_in)
                      | {desc = d} -> Printf.printf "%s%s\n" (Ident.name x) (Frame.type_structure e'.exp_type); Qualgen.add_label (xp, e'.exp_type)*)
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
  (* ming: subtyping may be better for the inner types when a complex type is
   * pulled up by name, but we have no way of expressing what we actually want,
   * which is that each element is of the same shape with an equality_refinement
   * *)
  (*| (Texp_ident _, {desc = Tconstr (p, l, _)}) ->
            (Frame.Fconstr (p, l, Builtins.equality_refinement (expression_to_pexpr e)),
             cstrs, framemap)*)
  | (Texp_ident (id, _), t) ->
            let (f', ftemplate) = (
                (try Lightenv.find id env
                  with Not_found -> fprintf std_formatter "@[Not_found:@ %s@]" (Path.unique_name id);
                       raise Not_found)
                  , Frame.fresh t) in
            (*let _ = fprintf std_formatter "@[instantiating@ %a@ %a@]" Frame.pprint f' Frame.pprint ftemplate in*)
            (*let _ = Frame.pprint Format.err_formatter f' in
            let _ = Frame.pprint Format.err_formatter ftemplate in *)
            let f = Frame.instantiate f' ftemplate in
              (f, WFFrame(env, f)::cstrs, framemap)
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
                in
                (*let _ = Format.printf "@[%a@\n@]" Frame.pprint f'' in*)
                (f'', SubFrame (env, guard, f2, f) :: cs', fm')
		          | _ -> assert false
            end
          | _ -> assert false
	        in List.fold_left constrain_application
                 (constrain e1 env guard cstrs framemap) exps
	| (Texp_let (Nonrecursive, [({pat_desc = Tpat_var x}, e1)], e2), t) ->
            let _ = if !under_lambda = 0 || not(!Clflags.less_qualifs) then Qualgen.add_label (Path.Pident x, e1.exp_type) 
                    else () in
            let lambda = match e1.exp_desc with
                      Texp_function (_, _) -> 
                        under_lambda := !under_lambda + 1; true 
                      | _ -> false in
            let (f1, cstrs'', fm'') = constrain e1 env guard cstrs framemap in
            let env' = Lightenv.add (Path.Pident x) f1 env in
            let _ = if lambda then under_lambda := !under_lambda - 1 else () in
            let (f2, cstrs', fm') = constrain e2 env' guard cstrs'' fm'' in
            let f = Frame.fresh_with_labels t f2 in
              (f, WFFrame(env', f) :: SubFrame (env', guard, f2, f) :: cstrs', fm')
	| (Texp_let (Recursive, bindings, body_exp), t) ->
            (* pmr: This is horrendous, but about the best we can do
               without using destructive updates.  We need to label
               the function we're binding in the environment where we
               also constrain the function.  Unfortunately, we don't
               have that label until after we constrain it!  So we do
               a first pass just to get the labels, then do a second
               with the proper labels added. *)
            let (vars, exprs) = List.split bindings in

            (* Determine the labels we need to have on our bound frames first *)
            let no_label_frame e = Frame.fresh e.exp_type in
            let unlabeled_frames = List.map no_label_frame exprs in
            let binding_var = function
              | {pat_desc = Tpat_var f} -> Path.Pident f
              | _ -> assert false
            in
            let vars = List.map binding_var vars in
            let unlabeled_env = Lightenv.addn (List.combine vars unlabeled_frames) env in
            let labeling_constraints = List.map (fun e -> constrain e unlabeled_env guard cstrs framemap) exprs in
            let (label_frames, _, _) = Misc.split3 labeling_constraints in
              (* pmr: I'm not assuming that constrain always gives us a fresh frame here, otherwise we could
                 use label_frames directly *)
            let binding_frames = List.map2 Frame.label_like unlabeled_frames label_frames in
            
              (* ming: qualgen code. generate qualifiers for all binding vars if
               * we're currently under a lambda build a bitmap for each bind to
               * manage lambda detection *)
            let qualgen_addlbls var exp = Qualgen.add_label (var, exp.exp_type) in
            let _ = if !under_lambda = 0 || not(!Clflags.less_qualifs) then List.iter2 qualgen_addlbls vars exprs  
                                                                       else () 
            in
            let qualgen_is_function e = 
              match e.exp_desc with
                Texp_function (_, _) -> true
                | _ -> false 
            in
            let qualgen_incr_lambda () = under_lambda := !under_lambda + 1 in
            let qualgen_decr_lambda () = under_lambda := !under_lambda - 1 in

            (* Redo constraints now that we know what the right labels are --- note that unlabeled_frames are all
               still essentially fresh, since we're discarding any constraints on them *)
            let bound_env = Lightenv.addn (List.combine vars binding_frames) env in
            let build_found_frame_list e (fframes, cs, fm) =
              let (frame, new_cs, new_fm) = constrain e bound_env guard cs fm in
                (frame :: fframes, new_cs, new_fm)
            in
              (* qualgen, continued.. wrap the fold function in another that
               * pushes onto the lambda stack while constraining functions *)
            let qualgen_wrap_found_frame_list e b = 
              if qualgen_is_function e then 
                let _ = qualgen_incr_lambda () in
                let r = build_found_frame_list e b in
                let _ = qualgen_decr_lambda () in
                  r  
              else
                    build_found_frame_list e b
            in
                  
            let (found_frames, cstrs1, fmap1) = List.fold_right qualgen_wrap_found_frame_list exprs ([], cstrs, framemap) in
            let (body_frame, cstrs2, fmap2) = constrain body_exp bound_env guard cstrs1 fmap1 in

            (* Ensure that the types we discovered for each binding are no more general than the types implied by
               their uses *)
            let build_found_frame_cstr_list cs found_frame binding_frame =
              WFFrame (bound_env, binding_frame) :: SubFrame (bound_env, guard, found_frame, binding_frame) :: cs
            in
            let binding_cstrs = List.fold_left2 build_found_frame_cstr_list cstrs2 found_frames binding_frames in
            let f = Frame.fresh_with_labels t body_frame in
              (f,
               WFFrame (bound_env, f)
               :: SubFrame (bound_env, guard, body_frame, f)
               :: binding_cstrs,
               fmap2)
(*

  pmr: is there a reason this stuff was hanging here rather than in a separate AST walker?

            let _ = if !under_lambda = 0 || not(!Clflags.less_qualifs) then Qualgen.add_label (Path.Pident f, e1.exp_type) 
            else () in
            let lambda = match e1.exp_desc with
                Texp_function (_, _) ->
                  under_lambda := !under_lambda + 1; true
              | _ -> false in

            let unlabelled_env = Lightenv.add fp unlabelled_f1 env in
            let (labelled_f1, _, _) = constrain e1 unlabelled_env guard cstrs framemap in
            let f1' = Frame.label_like unlabelled_f1 labelled_f1 in
            let env'' = Lightenv.add fp f1' env in
            let (f1, cstrs'', fm'') = constrain e1 env'' guard cstrs framemap in
            let env' = Lightenv.add fp f1 env in
            let _ = if lambda then under_lambda := !under_lambda - 1 else () in
            let (f2, cstrs', fm') = constrain e2 env' guard cstrs'' fm'' in
            let f = Frame.fresh_with_labels t f2 in
              (f,
               WFFrame (env', f)
               :: WFFrame (env'', f1')
               :: SubFrame (env', guard, f2, f)
               :: SubFrame (env'', guard, f1, f1')
               :: cstrs',
               fm') *)
	| (Texp_array(es), t) ->
            let _ = Qualgen.add_constant (List.length es) in
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
  | (Texp_tuple(es), t) ->
            (* placeholder implementation *)
            let e1 = List.hd es in
            let e2 = List.hd (List.tl es) in
            let (f1, c, m) = constrain e1 env guard cstrs framemap in
            let (f2, c, m) = constrain e2 env guard c m in
            let f = Frame.Ftuple(f1, f2) in
            begin
            match f with
              Frame.Ftuple(f1', f2') ->
                (f, List.append [WFFrame(env, f); SubFrame(env, guard, f1, f1'); SubFrame(env, guard, f2, f2')] 
                                c, m)
              | _ -> failwith "Texp_tuple has wrong type"
            end
  | (Texp_assertfalse, t) ->
      let f = Frame.fresh t in
        (f, cstrs, framemap)
	| (_, t) ->
      (* As it turns out, giving up and returning true here is actually _very_ unsound!  We won't check subexpressions! *)
      fprintf err_formatter "@[Warning:@ Don't@ know@ how@ to@ constrain@ expression,@ defaulting@ to@ true@ Ty_structure:@ %s@\n@]" (Frame.type_structure t);
      assert false
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

exception IllQualified of Frame.t LocationMap.t

let qualify_structure tenv fenv quals str =
  let (newquals, cstrs, fmap) = constrain_structure tenv fenv quals str in
  let instantiated_quals = instantiate_in_environments cstrs quals in
    Bstats.reset ();
    try
      let solution = Bstats.time "solving" (solve_constraints instantiated_quals) cstrs in
        Bstats.print stdout "\n\nTime to solve constraints:\n";
        (newquals, LocationMap.map (Frame.apply_solution solution) fmap)
    with Unsatisfiable solution ->
      raise (IllQualified (LocationMap.map (Frame.apply_solution solution) fmap))
