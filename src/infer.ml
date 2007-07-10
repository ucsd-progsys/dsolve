open Expr
open Type
open Predicate
open Constraint


let rec const_subst_tyvar b a = function
    [] ->
      []
  | (t1, t2)::cs ->
      (typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)::(const_subst_tyvar b a cs)


let rec occurs a = function
    TyVar a' ->
      a = a'
  | GenVar _ ->
      false
  | Int _ ->
      false
  | Arrow(_, t, t') ->
      (occurs a t) || (occurs a t')



let id_subst t = t

let update_subst t a subst = fun t' -> subst (typ_subst_tyvar t a t')

exception Unify

let rec unify = function
    [] ->
      id_subst
  | (t1, t2)::cs ->
      match (t1, t2) with
          (GenVar _, _)
        | (_, GenVar _) ->
            failwith "We have no business unifying genvars"
	| (TyVar a, t)
	| (t, TyVar a) ->
	    if (not (occurs a t)) || t1 = t2 then
	      let cs' = const_subst_tyvar t a cs in
                update_subst t a (unify cs')
	    else
	      raise Unify
	| (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
	    unify ((t1, t2)::(t1', t2')::cs)
	| (Int _, Int _) ->
	    unify cs
	| _ ->
	    raise Unify


let fresh_bindvar = Misc.make_get_fresh (fun x -> "_" ^ x)


module Expr = struct
  type t = expr
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (==)
end


module ExprMap = Map.Make(Expr)


let maplist f sm =
  ExprMap.fold (fun k v r -> (f k v)::r) sm []


let pprint_shapemap smap =
  let shapes = maplist (fun e t -> (pprint_expr e) ^ "\n::> " ^ (pprint_type t)) smap in
    Misc.join shapes "\n\n"


let infer_shape exp =
  let rec infer_rec e tenv constrs shapemap =
    match e with
	Num(_, _) ->
	  (Int [], constrs, shapemap)
      | ExpVar(x, _) ->
	  let tx = instantiate_type (List.assoc x tenv) in
	    (tx, constrs, shapemap)
      | If(c, e1, e2, _) ->
	  let (tc, constrsc, sm) = infer_mono c tenv constrs shapemap in
	  let (t1, constrs1, sm') = infer_mono e1 tenv constrsc sm in
	  let (t2, constrs2, sm'') = infer_mono e2 tenv constrs1 sm' in
	  let t = fresh_tyvar() in
	    (t, (t1, t)::(t2, t)::(tc, Int [])::constrs2, sm'')
      | App(e1, e2, _) ->
	  let (t1, constrs1, sm') = infer_mono e1 tenv constrs shapemap in
	  let (t2, constrs2, sm'') = infer_mono e2 tenv constrs1 sm' in
	  let returnty = fresh_tyvar() in
	  let funty = Arrow(fresh_bindvar(), t2, returnty) in
	    (returnty, (t1, funty)::constrs2, sm'')
      | Abs(x, _, e, _) ->
	  let t = fresh_tyvar () in
	  let newtenv = (x, t)::tenv in
	  let (t', constrs, sm') = infer_mono e newtenv constrs shapemap in
	    (Arrow(x, t, t'), constrs, sm')
      | Let(x, _, ex, e, _) ->
	  let (tx, constrsx, sm') = infer_general ex tenv constrs shapemap in
	  let newtenv = (x, tx)::tenv in
	  let (te, constrse, sm'') = infer_mono e newtenv constrsx sm' in
	    (te, constrse, sm'')
      | LetRec(f, _, ex, e, _) ->
          let tf = fresh_tyvar() in
          let tenv' = (f, tf)::tenv in
          let (tf', constrs'', sm'') = infer_mono ex tenv' constrs shapemap in
          let (te, constrs', sm') = infer_mono e tenv' constrs'' sm'' in
            (te, (tf, tf')::constrs', sm')
      | Cast(_, _, e, _) ->
          infer_mono e tenv constrs shapemap
  and infer_mono e tenv constrs shapemap =
    let (t, cs, sm) = infer_rec e tenv constrs shapemap in
      (t, cs, ExprMap.add e t sm)
  and infer_general e tenv constrs shapemap =
    let (t'', cs, sm) = infer_rec e tenv constrs shapemap in
    let sub = unify cs in
    let (t', tenv') = (sub t'', List.map (fun (a, ty) -> (a, sub ty)) tenv) in
    let t = generalize_type t' tenv' in
      (t, cs, ExprMap.add e t sm)
  in
  let (_, constrs, smap') = infer_mono exp Builtins.types [] ExprMap.empty in
  let sub = unify constrs in
    ExprMap.map sub smap'


let subtype_constraints exp quals shapemap =
  let fresh_frame e =
    let rec fresh_frame_rec = function
        Arrow(x, t, t') ->
	  FArrow(x, fresh_frame_rec t, fresh_frame_rec t')
      | TyVar a ->
          FVar([], a)
      | GenVar a ->
          FGenVar a
      | Int _ ->
	  fresh_framevar()
    in
      fresh_frame_rec (ExprMap.find e shapemap)
  in
  let rec constraints_rec e env guard constrs framemap =
    let (f, cs, fm) =
      match e with
	  Num(n, _) ->
            let f' = FInt([], [Builtins.equality_qualifier (PInt n)]) in
            let f = fresh_frame e in
	      (f, SubType(env, guard, f', f)::constrs, framemap)
        | ExpVar(x, _) ->
            let feq =
              match ExprMap.find e shapemap with
                  Int _ ->
                    FInt([], [Builtins.equality_qualifier (Var x)])
                | _ ->
                    instantiate_frame (List.assoc x env)
            in
            let f = fresh_frame e in
	      (f, SubType(env, guard, feq, f)::constrs, framemap)
        | Abs(x, _, e', _) ->
	    begin match fresh_frame e with
	        FArrow(_, f, f') ->
		  let env' = (x, f)::env in
		  let (f'', constrs', fm') = constraints_rec e' env' guard constrs framemap in
		    (FArrow(x, f, f'), SubType(env, guard, f'', f')::constrs', fm')
	      | _ ->
		  failwith "Fresh frame has wrong shape - expected arrow"
	    end
        | App(e1, e2, _) ->
	    begin match constraints_rec e1 env guard constrs framemap with
	        (FArrow(x, f, f'), constrs', fm') ->
		  let (f2, constrs'', fm'') = constraints_rec e2 env guard constrs' fm' in
		  let pe2 = expr_to_predicate_expression e2 in
		  let f'' = frame_apply_subst pe2 x f' in
		    (f'', SubType(env, guard, f2, f)::constrs'', fm'')
	      | _ ->
		  failwith "Subexpression frame has wrong shape - expected arrow"
	    end
        | If(e1, e2, e3, _) ->
            let f = fresh_frame e in
            let (f1, constrs''', fm''') = constraints_rec e1 env guard constrs framemap in
            let guardvar = fresh_bindvar() in
            let env' = (guardvar, f1)::env in
            let guardp = equals(Var guardvar, PInt 1) in
            let guard2 = And(guardp, guard) in
            let (f2, constrs'', fm'') = constraints_rec e2 env' guard2 constrs''' fm''' in
            let guard3 = And(Not(guardp), guard) in
            let (f3, constrs', fm') = constraints_rec e3 env' guard3 constrs'' fm'' in
              (f, SubType(env', guard2, f2, f)::SubType(env', guard3, f3, f)::constrs', fm')
        | Let(x, _, e1, e2, _) ->
            (* We don't need to generalize here because it's already been done
               by fresh_frame - whatever genvars are in the shape are passed
               over to the new frame *)
            let (f1, constrs'', fm'') = constraints_rec e1 env guard constrs framemap in
            let env' = (x, f1)::env in
            let (f2, constrs', fm') = constraints_rec e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::constrs', fm')
        | LetRec(f, _, e1, e2, _) ->
            let f1 = fresh_frame e1 in
            let env' = (f, f1)::env in
            let (f1', constrs'', fm'') = constraints_rec e1 env' guard constrs framemap in
            let (f2, constrs', fm') = constraints_rec e2 env' guard constrs'' fm'' in
            let f = fresh_frame e in
              (f, SubType(env', guard, f2, f)::SubType(env', guard, f1', f1)::constrs', fm')
        | Cast(t1, t2, e, _) ->
            let (f, constrs', fm') = constraints_rec e env guard constrs framemap in
            let (f1, f2) = (type_to_frame t1, type_to_frame t2) in
              (f2, SubType(env, guard, f, f1)::constrs', fm')
    in
      (f, cs, ExprMap.add e f fm)
  in
    constraints_rec exp Builtins.frames True [] ExprMap.empty


let infer_types exp quals =
  let shapemap = infer_shape exp in
  let builtin_quals = expr_required_builtin_quals exp in
  let qs = builtin_quals@quals in
  let (fr, constrs, fmap) = subtype_constraints exp qs shapemap in
  let solution = solve_constraints qs constrs in
  let _ = Printf.printf "%s\n\n" (pprint_frame fr) in
  let expr_to_type e =
    frame_to_type (frame_apply_solution solution (ExprMap.find e fmap))
  in
    expr_to_type
