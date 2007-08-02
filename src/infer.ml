open Asttypes
open Expr
open Parsetree
open Longident
open Type
open Frame
open Predicate
open Constraint


let rec const_subst_tyvar b a (t1, t2) = Misc.tmap2 (typ_subst_tyvar b a) (t1, t2)


let rec occurs a = function
    TyVar a' ->
      a = a'
  | GenTy _ ->
      failwith "Should not be trying to unify gentypes"
  | Int ->
      false
  | List t ->
      occurs a t
  | Arrow(_, t, t') ->
      (occurs a t) || (occurs a t')


let id_subst t = t

let update_subst t a subst = fun t' -> subst (typ_subst_tyvar t a t')

exception Unify

let rec unify = function
    [] -> id_subst
  | (t1, t2)::cs ->
      match (t1, t2) with
          (GenTy _, _)
        | (_, GenTy _) -> failwith "We have no business unifying gentys"
	| (TyVar a, t)
	| (t, TyVar a) ->
	    if (not (occurs a t)) || t1 = t2 then
	      let cs' = List.map (const_subst_tyvar t a) cs in
                update_subst t a (unify cs')
	    else raise Unify
	| (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
	    unify ((t1, t2)::(t1', t2')::cs)
        | (List t1, List t2) ->
            unify ((t1, t2)::cs)
	| (Int, Int) ->
	    unify cs
	| _ -> raise Unify


let fresh_bindvar = Misc.make_get_fresh (fun x -> "_" ^ x)

(* pmr: must remove redundant definitions of maplist *)
let maplist f sm = ExpMap.fold (fun k v r -> (f k v)::r) sm []

let pprint_shapemap smap =
  let shapes = maplist (fun e t -> (pprint_expression e) ^ "\n::> " ^ (pprint_typ t)) smap in
    Misc.join shapes "\n\n"


let infer_shapes exp =
  let rec infer_rec e tenv constrs shapemap =
    match e.pexp_desc with
	Pexp_constant(Const_int _) ->
	  (Int, constrs, shapemap)
      | Pexp_ident(Lident x) ->
	  (instantiate_typ (Env.find x tenv), constrs, shapemap)
      | Pexp_construct(Lident "[]", _, _) ->
          (List (fresh_tyvar ()), constrs, shapemap)
      | Pexp_construct(Lident "::", Some {pexp_desc = Pexp_tuple [e1; e2]}, _) ->
          let (t1, constrs'', sm'') = infer_mono e1 tenv constrs shapemap in
          let (t2, constrs', sm') = infer_mono e2 tenv constrs'' sm'' in
          let t = List t1 in
            (t, (t, t2)::constrs', sm')
      | Pexp_ifthenelse(c, e1, Some e2) ->
	  let (tc, constrsc, sm) = infer_mono c tenv constrs shapemap in
	  let (t1, constrs1, sm') = infer_mono e1 tenv constrsc sm in
	  let (t2, constrs2, sm'') = infer_mono e2 tenv constrs1 sm' in
	    (t1, (t1, t2)::(tc, Int)::constrs2, sm'')
(* pmr: well, this could be tedious... *)
(*      | Exp.Match(e1, e2, (h, t), e3, _) ->
          let (t1, constrs''', sm''') = infer_mono e1 tenv constrs shapemap in
          let (t2, constrs'', sm'') = infer_mono e2 tenv constrs''' sm''' in
          let tl = fresh_tyvar () in
          let tenv' = Env.addn [(h, tl); (t, List tl)] tenv in
          let (t3, constrs', sm') = infer_mono e3 tenv' constrs'' sm'' in
            (t3, (t1, List tl)::(t2, t3)::constrs', sm') *)
      | Pexp_apply(e, exps) ->
	  let type_application (t1, cs, sm) (_, e2) =
	    let (t2, cs'', sm'') = infer_mono e2 tenv cs sm in
	    let returnty = fresh_tyvar() in
	    let funty = Arrow(fresh_bindvar(), t2, returnty) in
	      (returnty, (t1, funty)::cs'', sm'')
	  in List.fold_left type_application (infer_mono e tenv constrs shapemap) exps
      | Pexp_function(_, _, [({ppat_desc = Ppat_var x}, e)]) ->
	  let t = fresh_tyvar () in
	  let (t', constrs, sm') = infer_mono e (Env.add x t tenv) constrs shapemap in
	    (Arrow(x, t, t'), constrs, sm')
      | Pexp_let(Nonrecursive, [({ppat_desc = Ppat_var x}, ex)], e) ->
	  let (tx, constrsx, sm') = infer_general ex tenv constrs shapemap in
            infer_mono e (Env.add x tx tenv) constrsx sm'
      | Pexp_let(Recursive, [({ppat_desc = Ppat_var f}, ef)], e) ->
          let (tf', constrs'', sm'') = infer_rec_general ef f tenv constrs shapemap in
            infer_mono e (Env.add f tf' tenv) constrs'' sm''
      | _ -> raise ExpressionNotSupported
  and infer_mono e tenv constrs shapemap =
    let (t, cs, sm) = infer_rec e tenv constrs shapemap in (t, cs, ExpMap.add e t sm)
  and infer_general e tenv constrs shapemap =
    let (t'', cs, sm) = infer_rec e tenv constrs shapemap in
    let sub = unify cs in
    let (t', tenv') = (sub t'', Env.mapi (fun a ty -> sub ty) tenv) in
    let t = generalize_typ t' tenv' in (t, cs, ExpMap.add e t sm)
  and infer_rec_general e f tenv constrs shapemap =
    (* A version of infer_general which makes sure to leave the tyvars introduced
       by the let out of the environment while generalizing (otherwise stuff breaks). *)
    let tf = fresh_tyvar() in
    let tenv'' = Env.add f tf tenv in
    let (t'', cs', sm) = infer_rec e tenv'' constrs shapemap in
    let cs = (tf, t'')::cs' in
    let sub = unify cs in
    let (t', tenv') = (sub t'', Env.mapi (fun a ty -> sub ty) tenv) in
    let t = generalize_typ t' tenv' in (t, cs, ExpMap.add e t sm)
  in
  let (_, constrs, smap') = infer_mono exp Builtins.types [] ExpMap.empty in
    ExpMap.map (unify constrs) smap'


let subtype_constraints exp quals shapemap =
  let fresh_frame e = fresh_frame_from_typ (ExpMap.find e shapemap) in
  let rec constraints_rec e env guard constrs framemap =
    let (f, cs, fm) =
      match e.pexp_desc with
	  Pexp_constant(Const_int n) ->
	    (FInt([], [Builtins.equality_qualifier (PInt n)]), constrs, framemap)
	| Pexp_ident(Lident x) ->
            let f =
              match ExpMap.find e shapemap with
                  Int ->
                    FInt([], [Builtins.equality_qualifier (Var x)])
                | _ ->
                    instantiate_frame (Env.find x env)
            in (f, constrs, framemap)
	| Pexp_construct(Lident "[]", _, _) ->
            (fresh_frame e, constrs, framemap)
	| Pexp_construct(Lident "::", Some {pexp_desc = Pexp_tuple [e1; e2]}, _) ->
            begin match fresh_frame e with
                FList f ->
                  let (f1, constrs'', fm'') = constraints_rec e1 env guard constrs framemap in
                    begin match constraints_rec e2 env guard constrs'' fm'' with
                        (FList f2, constrs', fm') ->
                          (FList f, SubType(env, guard, f1, f)::SubType(env, guard, f2, f)::constrs', fm')
                      | _ -> failwith "List tail frame has wrong shape"
                    end
              | _ -> failwith "Fresh frame has wrong shape - expected list"
            end
	| Pexp_function(_, _, [({ppat_desc = Ppat_var x}, e')]) ->
	    begin match fresh_frame e with
	        FArrow(_, f, f') ->
		  let env' = Env.add x f env in
		  let (f'', constrs', fm') = constraints_rec e' env' guard constrs framemap in
		    (FArrow(x, f, f'), SubType(env', guard, f'', f')::constrs', fm')
	      | _ -> failwith "Fresh frame has wrong shape - expected arrow"
	    end
	| Pexp_apply(e1, exps) ->
	    let constrain_application (f, cs, fm) (_, e2) =
	      match f with
		  FArrow(x, f, f') ->
		    let (f2, cs', fm') = constraints_rec e2 env guard cs fm in
		    let pe2 = expression_to_pexpr e2 in
		    let f'' = frame_apply_subst (x, pe2) f' in
		      (f'', SubType(env, guard, f2, f)::cs', fm')
		| _ -> failwith "Subexpression frame has wrong shape - expected arrow"
	    in List.fold_left constrain_application (constraints_rec e1 env guard constrs framemap) exps
	| Pexp_ifthenelse(e1, e2, Some e3) ->
            let f = fresh_frame e in
            let (f1, constrs''', fm''') = constraints_rec e1 env guard constrs framemap in
            let guardvar = fresh_bindvar() in
            let env' = Env.add guardvar f1 env in
            let guardp = equals(Var guardvar, PInt 1) in
            let guard2 = And(guardp, guard) in
            let (f2, constrs'', fm'') = constraints_rec e2 env' guard2 constrs''' fm''' in
            let guard3 = And(Not(guardp), guard) in
            let (f3, constrs', fm') = constraints_rec e3 env' guard3 constrs'' fm'' in
              (f, SubType(env', guard2, f2, f)::SubType(env', guard3, f3, f)::constrs', fm')
(*        | Exp.Match(e1, e2, (h, t), e3, _) ->
            begin match constraints_rec e1 env guard constrs framemap with
                (FList f1, constrs''', fm''') ->
                  let (f2, constrs'', fm'') = constraints_rec e2 env guard constrs''' fm''' in
                  let env' = Env.addn [(h, f1); (t, FList f1)] env in
                  let (f3, constrs', fm') = constraints_rec e3 env' guard constrs'' fm'' in
                  let f = fresh_frame e in
                    (f, SubType(env, guard, f2, f)::SubType(env', guard, f3, f)::constrs', fm')
              | _ -> failwith "Wrong shape for match guard - expected list"
            end *)
	| Pexp_let(Nonrecursive, [({ppat_desc = Ppat_var x}, e1)], e2) ->
            let (f1, constrs'', fm'') = constraints_rec e1 env guard constrs framemap in
            let env' = Env.add x (generalize_frame_like_typ f1 (ExpMap.find e1 shapemap)) env in
            let (f2, constrs', fm') = constraints_rec e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::constrs', fm')
	| Pexp_let(Recursive, [({ppat_desc = Ppat_var f}, e1)], e2) ->
            let f1'' = fresh_frame e1 in
            let env'' = Env.add f f1'' env in
            let (f1', constrs'', fm'') = constraints_rec e1 env'' guard constrs framemap in
            let env' = Env.add f (generalize_frame_like_typ f1' (ExpMap.find e1 shapemap)) env in
            let (f2, constrs', fm') = constraints_rec e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::SubType(env'', guard, f1', f1'')::constrs', fm')
	| _ -> raise ExpressionNotSupported
    in (f, cs, ExpMap.add e f fm)
  in constraints_rec exp Builtins.frames True [] ExpMap.empty


let infer_frames exp quals =
  let shapemap = infer_shapes exp in
  let qs = Env.addn (expression_required_builtin_quals exp) quals in
  let (fr, constrs, fmap) = subtype_constraints exp qs shapemap in
  let solution = solve_constraints qs constrs in
    ExpMap.map (frame_apply_solution solution) fmap
