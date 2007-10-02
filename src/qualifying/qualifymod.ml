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

let constrain_expression tenv quals exp cstrs initframemap =
  let rec constrain e env guard constrs framemap =
    let (f, cs, fm) =
      match (e.exp_desc, (repr e.exp_type).desc) with
	  (Texp_constant(Const_int n), Tconstr(path, [], _)) ->
            (ref (Fconstr(path, [], Builtins.equality_refinement (PInt n))),
             constrs, framemap)
        | (Texp_construct(cstrdesc, []), Tconstr(path, [], _)) ->
            let cstrref =
              match cstrdesc.cstr_tag with
                  Cstr_constant n -> Builtins.equality_refinement (PInt n)
                | _ -> Builtins.empty_refinement
            in (ref (Fconstr(path, [], cstrref)), constrs, framemap)
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
(*	| (Texp_function(_, _, [({ppat_desc = Ppat_var x}, e')]), _) ->
	    begin match fresh e.exp_type with
                FArrow(f, f') ->
                  let env' = Lightenv.add x f env in
                  let (f'', constrs', fm') = constrain e' env' guard cstrs framemap in
                    (FArrow(f, f'), SubType(env', guard, f'', f')::constrs', fm')
              | _ -> assert false
	    end *)
(*	| Pexp_ident(Lident x) ->
            let f =
              let ty = ExpMap.find e shapemap in
              match ty with
                  Base(b) ->
                    FBase(b, ([], RQuals [Builtins.equality_qualifier (Var x)]))
                | _ ->
                    instantiate_frame_like_typ (Env.find x env) ty
            in (f, constrs, framemap)
	| Pexp_construct(Lident "[]", _, _) ->
            (fresh_frame e, constrs, framemap)
	| Pexp_construct(Lident "::", Some {pexp_desc = Pexp_tuple [e1; e2]}, _) ->
            begin match fresh_frame e with
                FList f ->
                  let (f1, constrs'', fm'') = constrain e1 env guard constrs framemap in
                    begin match constrain e2 env guard constrs'' fm'' with
                        (FList f2, constrs', fm') ->
                          (FList f, SubType(env, guard, f1, f)::SubType(env, guard, f2, f)::constrs', fm')
                      | _ -> failwith "List tail frame has wrong shape"
                    end
              | _ -> failwith "Fresh frame has wrong shape - expected list"
            end

	| Pexp_apply(e1, exps) ->
	    let constrain_application (f, cs, fm) (_, e2) =
	      match f with
		  FArrow(x, f, f') ->
		    let (f2, cs', fm') = constrain e2 env guard cs fm in
		    let pe2 = expression_to_pexpr e2 in
		    let f'' = frame_apply_subst (x, pe2) f' in
		      (f'', SubType(env, guard, f2, f)::cs', fm')
		| _ -> failwith "Subexpression frame has wrong shape - expected arrow"
	    in List.fold_left constrain_application (constrain e1 env guard constrs framemap) exps
(*        | Exp.Match(e1, e2, (h, t), e3, _) ->
            begin match constrain e1 env guard constrs framemap with
                (FList f1, constrs''', fm''') ->
                  let (f2, constrs'', fm'') = constrain e2 env guard constrs''' fm''' in
                  let env' = Env.addn [(h, f1); (t, FList f1)] env in
                  let (f3, constrs', fm') = constrain e3 env' guard constrs'' fm'' in
                  let f = fresh_frame e in
                    (f, SubType(env, guard, f2, f)::SubType(env', guard, f3, f)::constrs', fm')
              | _ -> failwith "Wrong shape for match guard - expected list"
            end *)
	| Pexp_let(Nonrecursive, [({ppat_desc = Ppat_var x}, e1)], e2) ->
            let (f1, constrs'', fm'') = constrain e1 env guard constrs framemap in
            let env' = Env.add x f1 env in
            let (f2, constrs', fm') = constrain e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::constrs', fm')
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
  in constrain exp Lightenv.empty True cstrs initframemap

(* pmr: note we're operating in the environment created by typing the
   structure - it's entirely possible this has some bad corner cases *)
let constrain_structure tenv initquals str =
  let rec constrain_rec quals cstrs fmap = function
    | [] -> (quals, cstrs, fmap)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs', fmap') = constrain_expression tenv quals exp cstrs fmap in
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

let qualify_structure tenv quals str =
  let (newquals, cstrs, fmap) = constrain_structure tenv quals str in
  let instantiated_quals = instantiate_in_environments cstrs quals in
  let solution = solve_constraints instantiated_quals cstrs in
    (newquals, LocationMap.map (frame_apply_solution solution) fmap)
