open Expr
open Type
open Env


exception Unify


type typconst =
    TypEq of typ * typ


let rec const_subst_tyvar b a = function
    [] ->
      []
  | TypEq(t1, t2)::cs ->
      TypEq(typ_subst_tyvar b a t1,
	    typ_subst_tyvar b a t2)::(const_subst_tyvar b a cs)


let rec occurs a = function
    TyVar a' ->
      a = a'
  | Int
  | Nil ->
      false
  | Arrow(t, t') ->
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
	| (Arrow(t1, t1'), Arrow(t2, t2')) ->
	    unify (TypEq(t1, t2)::TypEq(t1', t2')::cs)
	| (Int, Int) ->
	    unify cs
	| _ ->
	    raise Unify


let type_vars typ =
  let rec type_vars_rec t vars =
    match t with
	TyVar a ->
	  a::vars
      | Nil
      | Int ->
	  []
      | Arrow(t1, t2) ->
	  let vars' = type_vars_rec t1 vars in
	    type_vars_rec t2 vars'
  in
    type_vars_rec typ []


let nexttyvar = ref (Char.code 'a')
let get_fresh_tyvar () =
  let id = Char.escaped (Char.chr !nexttyvar) in
    incr nexttyvar;
    TyVar id


let infer_type e =
  let rec infer_rec e tenv constrs =
    match e with
	Num(_, _) ->
	  (Int, constrs)
      | ExpVar(x, _) ->
	  let tx = env_lookup x tenv in
	    (tx, constrs)
      | BinOp(_, e1, e2, _)
      | BinRel(_, e1, e2, _) ->
	  let (t1, constrs1) = infer_rec e1 tenv constrs in
	  let (t2, constrs2) = infer_rec e2 tenv constrs1 in
	    (Int, TypEq(t1, Int)::TypEq(t2, Int)::constrs2)
      | If(c, e1, e2, _) ->
	  let (tc, constrsc) = infer_rec c tenv constrs in
	  let (t1, constrs1) = infer_rec e1 tenv constrsc in
	  let (t2, constrs2) = infer_rec e2 tenv constrs1 in
	  let t = get_fresh_tyvar () in
	    (t, TypEq(t1, t)::TypEq(t2, t)::TypEq(tc, Int)::constrs2)
      | App(e1, e2, _) ->
	  let (t1, constrs1) = infer_rec e1 tenv constrs in
	  let (t2, constrs2) = infer_rec e2 tenv constrs1 in
	  let returnty = get_fresh_tyvar () in
	  let funty = Arrow(t2, returnty) in
	    (returnty, TypEq(t1, funty)::constrs2)
      | Abs(x, _, e, _) ->
	  let t = get_fresh_tyvar () in
	  let newtenv = env_add x t tenv in
	  let (t', constrs) = infer_rec e newtenv constrs in
	    (Arrow(t, t'), constrs)
      | Let(x, _, ex, e, _) ->
	  let (tx, constrsx) = infer_rec ex tenv constrs in
	  let newtenv = env_add x tx tenv in
	  let (te, constrse) = infer_rec e newtenv constrsx in
	    (te, constrse)
  in
  let (t, constrs) = infer_rec e [] [] in
  let sub = unify constrs in
    sub t

