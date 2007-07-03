open Expr
open Type
open Predicate
open Constraint


exception Unify


type typconst = TypEq of typ * typ


let rec const_subst_tyvar b a = function
    [] ->
      []
  | TypEq(t1, t2)::cs ->
      TypEq(typ_subst_tyvar b a t1,
	    typ_subst_tyvar b a t2)::(const_subst_tyvar b a cs)


let rec occurs a = function
    TyVar a' ->
      a = a'
  | Int _ ->
      false
  | Arrow(_, t, t') ->
      (occurs a t) || (occurs a t')


let rec unify = function
    [] ->
      fun t -> t
  | TypEq(t1, t2)::cs ->
      match (t1, t2) with
	  (TyVar a, t)
	| (t, TyVar a) ->
	    if (not (occurs a t)) || t1 = t2 then
	      let cs' = const_subst_tyvar t a cs in
	      let unifsub = unify cs' in
		fun t' -> unifsub (typ_subst_tyvar t a t')
	    else
	      raise Unify
	| (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
	    unify (TypEq(t1, t2)::TypEq(t1', t2')::cs)
	| (Int _, Int _) ->
	    unify cs
	| _ ->
	    raise Unify


let type_vars typ =
  let rec type_vars_rec t vars =
    match t with
	TyVar a ->
	  a::vars
      | Int _ ->
	  []
      | Arrow(_, t1, t2) ->
	  let vars' = type_vars_rec t1 vars in
	    type_vars_rec t2 vars'
  in
    type_vars_rec typ []


let fresh_tyvar = Misc.make_get_fresh (fun x -> TyVar x)
let fresh_bindvar = Misc.make_get_fresh (fun x -> "_" ^ x)


module Expr = struct
  type t = expr
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (==)
end


module ShapeMap = Map.Make(Expr)


let infer_shape exp =
  let rec infer_rec e tenv constrs shapemap =
    let (t, cs, sm) =
      match e with
	  Num(_, _) ->
	    (Int [], constrs, shapemap)
	| ExpVar(x, _) ->
	    let tx = List.assoc x tenv in
	      (tx, constrs, shapemap)
	| If(c, e1, e2, _) ->
	    let (tc, constrsc, sm) = infer_rec c tenv constrs shapemap in
	    let (t1, constrs1, sm') = infer_rec e1 tenv constrsc sm in
	    let (t2, constrs2, sm'') = infer_rec e2 tenv constrs1 sm' in
	    let t = fresh_tyvar() in
	      (t, TypEq(t1, t)::TypEq(t2, t)::TypEq(tc, Int [])::constrs2, sm'')
	| App(e1, e2, _) ->
	    let (t1, constrs1, sm') = infer_rec e1 tenv constrs shapemap in
	    let (t2, constrs2, sm'') = infer_rec e2 tenv constrs1 sm' in
	    let returnty = fresh_tyvar() in
	    let funty = Arrow(fresh_bindvar(), t2, returnty) in
	      (returnty, TypEq(t1, funty)::constrs2, sm'')
	| Abs(x, _, e, _) ->
	    let t = fresh_tyvar () in
	    let newtenv = (x, t)::tenv in
	    let (t', constrs, sm') = infer_rec e newtenv constrs shapemap in
	      (Arrow(x, t, t'), constrs, sm')
	| Let(x, _, ex, e, _) ->
	    let (tx, constrsx, sm') = infer_rec ex tenv constrs shapemap in
	    let newtenv = (x, tx)::tenv in
	    let (te, constrse, sm'') = infer_rec e newtenv constrsx sm' in
	      (te, constrse, sm'')
    in
      (t, cs, ShapeMap.add e t sm)
  in
  let (t, constrs, smap') = infer_rec exp Builtins.types [] ShapeMap.empty in
  let sub = unify constrs in
  let smap = ShapeMap.map sub smap' in
    smap


let maplist f sm =
  ShapeMap.fold (fun k v r -> (f k v)::r) sm []


let pprint_shapes exp =
  let smap = infer_shape exp in
  let shapes = maplist (fun e t -> (pprint_expr e) ^ "\n::> " ^ (pprint_type t)) smap in
    Misc.join shapes "\n\n"


let subtype_constraints exp quals shapemap =
  let rec fresh_frame e =
    let rec fresh_frame_rec = function
	Arrow(x, t, t') ->
	  FArrow(x, fresh_frame_rec t, fresh_frame_rec t')
      | _ ->
	  fresh_framevar()
    in
      fresh_frame_rec (ShapeMap.find e shapemap)
  in
  let rec constraints_rec e env guard constrs =
    match e with
	Num(n, _) ->
	  (FInt([], const_int_quals quals guard n), constrs)
      | ExpVar(x, _) ->
	  (List.assoc x env, constrs)
      | Abs(x, _, e', _) ->
	  begin match fresh_frame e with
	      FArrow(_, f, _) ->
		let env' = (x, f)::env in
		let (f', constrs') = constraints_rec e' env' guard constrs in
		  (FArrow(x, f, f'), constrs')
	    | _ ->
		failwith "Fresh frame has wrong shape - expected arrow"
	  end
      | App(e1, e2, _) ->
	  begin match constraints_rec e1 env guard constrs with
	      (FArrow(x, f, f'), constrs') ->
		let (f2, constrs'') = constraints_rec e2 env guard constrs' in
		let pe2 = expr_to_predicate_expression e2 in
		let f'' = frame_apply_subst pe2 x f' in
		  (f'', SubType(env, guard, f2, f)::constrs'')
	    | _ ->
		failwith "Subexpression frame has wrong shape - expected arrow"
	  end
      | _ ->
	  failwith "Constraints for this expression not yet implemented"
  in
    constraints_rec exp Builtins.frames True []


let infer_type exp quals =
  let shapemap = infer_shape exp in
  let (fr, constrs) = subtype_constraints exp quals shapemap in
  let solution = solve_constraints quals constrs in
  let _ = Printf.printf "%s\n\n" (pprint_frame fr) in
    frame_to_type (frame_apply_solution solution fr)
