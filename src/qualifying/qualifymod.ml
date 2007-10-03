open Asttypes
open Typedtree
open Btype
open Types
open Frame
open Predicate
open Constraint
open Longident

module LocationMap = Map.Make(struct type t = Location.t
                                     let compare = compare end)

let expression_to_pexpr e =
  match e.exp_desc with
    | Texp_constant (Const_int n) ->
	PInt n
    | Texp_ident (id, _) ->
	Var (Path.head id)
    | _ ->
        Var (Ident.create "")

let name_lookup_hack path env =
  try
    List.assoc (Path.name path) Builtins.frames
  with Not_found ->
    (* pmr: This is intrinsically broken for any path that isn't a plain
       Pident.  The long-term fix is merge Lightenv and Env. *)
    Lightenv.find (Path.head path) env

let constrain_expression tenv initenv quals exp initcstrs initframemap =
  let rec constrain e env guard cstrs framemap =
    let (f, cs, fm) =
      match (e.exp_desc, repr e.exp_type) with
	  (Texp_constant(Const_int n), {desc = Tconstr(path, [], _)}) ->
            (ref (Fconstr(path, [], Builtins.equality_refinement (PInt n))),
             cstrs, framemap)
        | (Texp_construct(cstrdesc, []), {desc = Tconstr(path, [], _)}) ->
            let cstrref =
              match cstrdesc.cstr_tag with
                  Cstr_constant n -> Builtins.equality_refinement (PInt n)
                | _ -> Builtins.empty_refinement
            in (ref (Fconstr(path, [], cstrref)), cstrs, framemap)
	| (Texp_ifthenelse(e1, e2, Some e3), _) ->
            let f = fresh e.exp_type in
            let (f1, cstrs1, fm1) = constrain e1 env guard cstrs framemap in
            let guardvar = Ident.create "guard" in
            let true_tag =
              match (Env.lookup_constructor (Lident "true") tenv).cstr_tag with
                  Cstr_constant n -> n
                | _ -> assert false
            in
            let guardp = equals(Var guardvar, PInt true_tag) in
            let env' = Lightenv.add guardvar f1 env in
            let guard2 = And(guardp, guard) in
            let (f2, cstrs2, fm2) = constrain e2 env' guard2 cstrs1 fm1 in
            let guard3 = And(Not(guardp), guard) in
            let (f3, cstrs3, fm3) = constrain e3 env' guard3 cstrs2 fm2 in
              (f,
               WFFrame(env, f)
               ::SubFrame(env', guard2, f2, f)
               ::SubFrame(env', guard3, f3, f)
               ::cstrs3,
               fm3)
	| (Texp_function([({pat_desc = Tpat_var x}, e')], _), _) ->
	    begin match !(fresh e.exp_type) with
              | Farrow(_, f, unlabelled_f') ->
                  let env' = Lightenv.add x f env in
                  let (f'', cstrs', fm') = constrain e' env' guard cstrs framemap in
                  (* Since the underlying type system doesn't have dependent
                     types, fresh can't give us the proper labels for the RHS.
                     Instead, we have to label it after the fact. *)
                  let f' = label_like unlabelled_f' f'' in
                  let f = ref (Farrow (Some x, f, f')) in
                    (f,
                     WFFrame (env, f)
                     :: SubFrame (env', guard, f'', f')
                     :: cstrs',
                     fm')
              | _ -> assert false
	    end
	| (Texp_ident _, {desc = Tconstr (p, [], _)}) ->
            (ref (Fconstr (p, [],
                           Builtins.equality_refinement (expression_to_pexpr e))),
             cstrs, framemap)
        | (Texp_ident (id, _), t) ->
            (* pmr: Later, the env should probably take paths, not idents.
               Something to think about... *)
            let (f, ftemplate) = (name_lookup_hack id env, Frame.fresh t) in
              instantiate f ftemplate;
              (f, cstrs, framemap)
	| (Texp_apply (e1, exps), _) ->
	    let constrain_application (f, cs, fm) = function
              | (Some e2, _) ->
	          begin match !f with
		    | Farrow (l, f, f') ->
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
            let env' = Lightenv.add x f1 env in
            let (f2, cstrs', fm') = constrain e2 env' guard cstrs'' fm'' in
            let f = Frame.fresh t in
              (f, SubFrame (env', guard, f2, f) :: cstrs', fm')
(*
	| Pexp_let(Recursive, [({ppat_desc = Ppat_var f}, e1)], e2) ->
            let f1'' = fresh_frame e1 in
            let env'' = Env.add f f1'' env in
            let (f1', constrs'', fm'') = constrain e1 env'' guard constrs framemap in
            let env' = Env.add f f1' env in
            let (f2, constrs', fm') = constrain e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::SubType(env'', guard, f1', f1'')::constrs', fm')
	| _ -> raise ExpressionNotSupported *)
        | _ -> assert false
    in (f, cs, LocationMap.add e.exp_loc f fm)
  in constrain exp initenv True initcstrs initframemap

(* pmr: note we're operating in the environment created by typing the
   structure - it's entirely possible this has some bad corner cases *)
let constrain_structure tenv fenv initquals str =
  let rec constrain_rec quals cstrs fmap = function
    | [] -> (quals, cstrs, fmap)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs', fmap') = constrain_expression tenv fenv quals exp cstrs fmap in
          constrain_rec quals cstrs' fmap' srem
    | (Tstr_qualifier (name, (valu, pred))) :: srem ->
        let newquals = (Path.Pident name, valu, pred) :: quals in
          constrain_rec newquals cstrs fmap srem
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
    (newquals, LocationMap.map (frame_apply_solution solution) fmap)
