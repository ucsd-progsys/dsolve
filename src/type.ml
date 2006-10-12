open Env
open Expr
open Printf


exception TypeMismatch of expr * typ


let rec get_qualifier = function
    Arrow(q, _, _)
  | Int(q)
  | Bool(q)
  | TyVar(q, _) ->
      q
  | Nil ->
      Top
  | ForallQual(_, _, t) ->
      get_qualifier t
  | ForallTyp(_, t) ->
      get_qualifier t


let rec set_qualifier t q =
  match t with
      Arrow(_, t1, t2) ->
	Arrow(q, t1, t2)
    | Int(_) ->
	Int(q)
    | Bool(_) ->
	Bool(q)
    | TyVar(_, a) ->
	TyVar(q, a)
    | Nil ->
	Nil
    | ForallQual(k, q, t) ->
	ForallQual(k, q, set_qualifier t q)
    | ForallTyp(a, t) ->
	ForallTyp(a, set_qualifier t q)


let annotate_type t q =
  set_qualifier t (QualMeet(get_qualifier t, q))


let rec qual_subst_qualvar q k = function
    QualVar(k') when k = k' ->
      q
  | QualMeet(q1, q2) ->
      QualMeet(qual_subst_qualvar q k q1, qual_subst_qualvar q k q2)
  | QualJoin(q1, q2) ->
      QualJoin(qual_subst_qualvar q k q1, qual_subst_qualvar q k q2)
  | q ->
      q


let rec typ_subst_qualvar q k = function
    ForallTyp(a, t) ->
      ForallTyp(a, typ_subst_qualvar q k t)
  | ForallQual(k', b, t) when k <> k' ->
      ForallQual(k', b, typ_subst_qualvar q k t)
  | ForallQual _ as t ->
      t
  | Arrow(q', t, t') ->
      Arrow(qual_subst_qualvar q k q', typ_subst_qualvar q k t, typ_subst_qualvar q k t')
  | Bool(q') ->
      Bool(qual_subst_qualvar q k q')
  | Int(q') ->
      Int(qual_subst_qualvar q k q')
  | TyVar(q', a) ->
      TyVar(qual_subst_qualvar q k q', a)
  | Nil ->
      Nil


let rec typ_subst_tyvar b a = function
    ForallTyp(a', t) when a <> a' ->
      ForallTyp(a', typ_subst_tyvar b a t)
  | ForallQual(k, q, t) ->
      ForallQual(k, q, typ_subst_tyvar b a t)
  | Arrow(q, t1, t2) ->
      Arrow(q, typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)
  | TyVar(q, a') when a' = a ->
      (* Preserve the existing qualifier and just replace the type variable portion *)
      begin match b with
	  TyVar(_, b) ->
	    TyVar(q, b)
	| Int(_) ->
	    Int(q)
	| Bool(_) ->
	    Bool(q)
	| t ->
	    t
      end
  | t ->
      t


let rec qualifier_less_equal q1 q2 =
  match (q1, q2) with
      (QualJoin(j1, j2), _) ->
	qualifier_less_equal j1 q2 &&
	  qualifier_less_equal j2 q2
    | (QualMeet(m1, m2), _) ->
	qualifier_less_equal m1 q2 ||
	  qualifier_less_equal m2 q2
    | (_, QualJoin(j1, j2)) ->
	qualifier_less_equal q1 j1 ||
	  qualifier_less_equal q1 j2
    | (_, QualMeet(m1, m2)) ->
	qualifier_less_equal q1 m1 &&
	  qualifier_less_equal q1 m2
    | (QualVar(k1), QualVar(k2)) ->
	k1 = k2
    | (_, Top) ->
	true
    | (Bottom, _) ->
	true
    | (q1, q2) ->
	q1 = q2


(* Type checking *)
let rec subtype t1 t2 =
  match (t1, t2) with
      (Arrow(q1, t1, t1'), Arrow(q2, t2, t2')) ->
	qualifier_less_equal q1 q2 &&
	  subtype t1' t2' &&
	  subtype t2 t1
    | (Int(q1), Int(q2))
    | (Bool(q1), Bool(q2)) ->
	qualifier_less_equal q1 q2
    | (TyVar(q1, s1), TyVar(q2, s2)) when s1 = s2 ->
	qualifier_less_equal q1 q2
    | (ForallQual(k1, q1, t1), ForallQual(k2, q2, t2)) ->
	if (qualifier_less_equal q2 q1) then
	  let t1 = typ_subst_qualvar q2 k1 t1 in
	  let t2 = typ_subst_qualvar q2 k2 t2 in
	    subtype t1 t2
	else
	  false
    | (ForallTyp(a1, t1), ForallTyp(a2, t2)) ->
	let t2 = typ_subst_tyvar (TyVar(Top, a1)) a2 t2 in
	  subtype t1 t2
    | _ ->
	false


let qualifiers_equal q1 q2 =
  qualifier_less_equal q1 q2 &&
    qualifier_less_equal q2 q1


(* While it would be nice to do types_equal analogously to qualifiers_equal, it
   doesn't work; the subtype relation is not antisymmetric.  (Is this a
   deficiency of the subtype relation?) *)
let rec types_equal t1 t2 =
  match (t1, t2) with
      (ForallTyp(a1, t1), ForallTyp(a2, t2)) ->
	let t2 = typ_subst_tyvar (TyVar(Top, a1)) a2 t2 in
	  types_equal t1 t2
    | (ForallQual(k1, q1, t1), ForallQual(k2, q2, t2)) ->
	let t2 = typ_subst_qualvar (QualVar k1) k2 t2 in
	  qualifiers_equal q1 q2 &&
	    types_equal t1 t2
    | (Arrow(q1, t1, t1'), Arrow(q2, t2, t2')) ->
	qualifiers_equal q1 q2 && types_equal t1 t2 && types_equal t1' t2'
    | (Int(q1), Int(q2))
    | (Bool(q1), Bool(q2)) ->
	qualifiers_equal q1 q2
    | (TyVar(q1, a1), TyVar(q2, a2)) when a1 = a2 ->
	qualifiers_equal q1 q2
    | _ ->
	false


let check_type e texp =
  let rec type_exp e texp tenv =
  match e with
      Num(_) ->
	Int(Top)
    | True
    | False ->
	Bool(Top)
    | Var(x) ->
	env_lookup x tenv
    | Let(x, tx, ex, e) ->
	let tx = match tx with
	    None ->
	      raise (TypeMismatch(e, texp))
	  | Some tx ->
	      tx
	in
	let tx' = type_exp ex tx tenv in
	  if subtype tx' tx then
	    let newtenv = env_add x tx tenv in
	      type_exp e texp newtenv
	  else
	    raise (TypeMismatch(ex, tx))
    | Abs(x, tx, e) ->
	let tx = match tx with
	    None ->
	      raise (TypeMismatch(e, texp))
	  | Some tx ->
	      tx
	in
	begin match texp with
	    Arrow(_, _, returnexp) ->
	      let newtenv = env_add x tx tenv in
	      let t' = type_exp e returnexp newtenv in
		Arrow(Top, tx, t')
	  | _ ->
		raise (TypeMismatch(e, texp))
	  end
    | App(e1, e2) ->
	(* Fake an expected type for the function being applied -
	   all we're really interested in is a proper return type
	   (see above) *)
	let expected_t1 =
	  Arrow(Top, TyVar(QualVar("k"), "a"), texp)
	in
	  begin match type_exp e1 expected_t1 tenv with
	      Arrow(q, t, t') ->
		let t2 = type_exp e2 t tenv in
		  if subtype t2 t then
		    t'
		  else
		    raise (TypeMismatch(e2, t'))
	    | t ->
		raise (TypeMismatch(e1, t))
	  end
    | QualAbs(k, q, e) ->
	begin match texp with
	    ForallQual(_, q', t) ->
	      if qualifiers_equal q q' then
		let t' = type_exp e t tenv in
		  ForallQual(k, q, t')
	      else
		raise (TypeMismatch(e, texp))
	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    | QualApp(e, q) ->
	(* If we were calculating the type for this thing afresh, we'd need a precise
	   expected type.  But since this has to appear in the body of a let, we're
	   only deconstructing an existing type at this point and we're ok. *)
	begin match type_exp e Nil tenv with
	    ForallQual(k, q', t) ->
	      if qualifier_less_equal q q' then
		typ_subst_qualvar q k t
	      else
		raise (TypeMismatch(e, texp))
	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    |	Annot(q, e) ->
	  (* XXX: *sniff* smells like...  bogitude - but we don't know what type's being annotated,
	     so I have no idea what can be done about removing the annotation *)
	  let t = type_exp e texp tenv in
	    annotate_type t q
    | If(c, e1, e2) ->
	begin match type_exp c (Bool(Top)) tenv with
	    Bool(Top) ->
	      let t1 = type_exp e1 texp tenv in
	      let t2 = type_exp e2 texp tenv in
		if (subtype t1 texp) && (subtype t2 texp) then
		  texp
		else
		  raise (TypeMismatch(e, texp))
   	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    | TyAbs(a, e) ->
	begin match texp with
	    ForallTyp(_, t) ->
	      ForallTyp(a, type_exp e t tenv)
	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    | TyApp(e, t) ->
	(* We can get away with faking the expected type for the same reasons it's ok for
	   QualApp *)
	begin match (type_exp e Nil tenv) with
	    ForallTyp(a, s) ->
	      typ_subst_tyvar t a s
	  | t ->
	      raise (TypeMismatch(e, t))
	end
  in
    try
      let t = type_exp e texp [] in
	types_equal t texp
    with _ ->
      false


(* Type Inference
   (Abandon all hope, ye who scroll below)
 *)
exception Unify


type const =
    TypEq of typ * typ
  | QualEq of qual * qual
  | QualLessEq of qual * qual


let rec const_subst_tyvar b a = function
    [] ->
      []
  | TypEq(t1, t2)::cs ->
      TypEq(typ_subst_tyvar b a t1,
	    typ_subst_tyvar b a t2)::(const_subst_tyvar b a cs)
  | c::cs ->
      const_subst_tyvar b a cs


let rec const_subst_qualvar b a = function
    [] ->
      []
  | QualEq(q1, q2)::cs ->
      QualEq(qual_subst_qualvar b a q1, qual_subst_qualvar b a q2)::(const_subst_qualvar b a cs)
  | QualLessEq(q1, q2)::cs ->
      QualLessEq(qual_subst_qualvar b a q1, qual_subst_qualvar b a q2)::(const_subst_qualvar b a cs)
  | TypEq(t1, t2)::cs ->
      TypEq(typ_subst_qualvar b a t1, typ_subst_qualvar b a t2)::(const_subst_qualvar b a cs)


let rec occurs a = function
    TyVar(_, a') ->
      a = a'
  | Int(_)
  | Bool(_)
  | Nil ->
      false
  | Arrow(_, t, t') ->
      (occurs a t) || (occurs a t')
  | ForallQual(_, _, t) ->
      occurs a t
  | ForallTyp(_, t) ->
      occurs a t


let rec unify_rec = function
    [] ->
      fun t -> t
  | QualEq(q1, q2)::cs ->
      begin match (q1, q2) with
	  (QualVar k, q)
	| (q, QualVar k) ->
	    let unifsub = unify_rec (const_subst_qualvar q k cs) in
	      fun t -> unifsub (typ_subst_qualvar q k t)
	| _ ->
	    if qualifiers_equal q1 q2 then
	      unify_rec cs
	    else
	      raise Unify
      end
  | QualLessEq(q1, q2)::cs ->
      begin match q1 with
	  QualVar k ->
	    unify_rec (const_subst_qualvar q2 k cs)
	| _ ->
	    if qualifier_less_equal q1 q2 then
	      unify_rec cs
	    else
	      raise Unify
      end
  | TypEq(t1, t2)::cs ->
      match (t1, t2) with
	  (TyVar(_, a), t)
	| (t, TyVar(_, a)) ->
	    if (not (occurs a t)) || (types_equal t1 t2) then
	      let unifsub = unify_rec (const_subst_tyvar t a cs) in
		fun t' -> unifsub (typ_subst_tyvar t a t')
	    else
	      raise Unify
	| (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
	    begin match (t1, t2) with
		(Arrow _, Arrow _) ->
		  unify_rec (TypEq(t1, t2)::TypEq(t1', t2')::cs)
	      | _ ->
		  let (q1, q2) = (get_qualifier t1, get_qualifier t2) in
		    (* Qualifiers on the LHS of an arrow must be either literals
		       (and their combinations) or singular qualifier variables.
		       Here we ensure the type remains well-formed according to this
		       definition.  (Or so I hope!)
		    *)
		    begin match (q1, q2) with
			(q, QualVar k)
		      | (QualVar k, q) ->
			  unify_rec (QualEq(q1, q2)::TypEq(t1, t2)::TypEq(t1', t2')::cs)
		       | _ ->
			   unify_rec (TypEq(t1, t2)::TypEq(t1', t2')::cs)
		    end
	    end
	| (Bool(_), Bool(_))
	| (Int(_), Int(_)) ->
	    unify_rec cs
	| _ ->
	    raise Unify


let unify constrs =
  let eq_constrs_before_leq ql =
    let is_eq = function
	QualEq(_, _) ->
	  true
      | _ ->
	  false
    in
    let (eq, leq) = List.partition is_eq ql in
      eq @ leq
  in
    (* All QualLessEq constraints need to be at the end to ensure that all variables take their final
       values from unification of the QualEq constraints. *)
    unify_rec (eq_constrs_before_leq constrs)


let rec qual_vars = function
    QualVar q ->
      [q]
  | QualMeet(q1, q2)
  | QualJoin(q1, q2) ->
      (qual_vars q1) @ (qual_vars q2)
  | _ ->
      []


let rec type_vars = function
  TyVar(q, a) ->
    ([a], qual_vars q)
  | Arrow(q, t1, t2) ->
      let (t1typvars, t1qualvars) = type_vars t1 in
      let (t2typvars, t2qualvars) = type_vars t2 in
	(t1typvars @ t2typvars, (qual_vars q) @ t1qualvars @ t2qualvars)
  | Int(q)
  | Bool(q) ->
      ([], qual_vars q)
  | Nil ->
      ([], [])
  | ForallQual(_, _, qs) ->
      type_vars qs
  | ForallTyp(_, t) ->
      type_vars t


let rec type_bound_vars = function
    ForallTyp(a, t) ->
      let (typboundvars, qualboundvars) = type_bound_vars t in
	(a::typboundvars, qualboundvars)
  | ForallQual(q, _, t) ->
      let (typboundvars, qualboundvars) = type_bound_vars t in
	(typboundvars, q::qualboundvars)
  | _ ->
      ([], [])


let set_minus a b =
  List.filter (fun v -> not (List.mem v b)) a


let type_free_vars t =
  let (typboundvars, qualboundvars) = type_bound_vars t in
  let (typvars, qualvars) = type_vars t in
    (set_minus typvars typboundvars,
     set_minus qualvars qualboundvars)


let nexttyvar = ref (Char.code 'a')
let nextqualvar = ref (Char.code 'A')


let getfreshqualvar () =
  let id = Char.escaped(Char.chr !nextqualvar) in
    incr nextqualvar;
    QualVar id


let getfreshvar () =
  let id = Char.escaped(Char.chr !nexttyvar) in
    incr nexttyvar;
    TyVar(getfreshqualvar (), id)


let rec env_free_vars = function
    [] ->
      ([], [])
  | EnvVar(_, t)::es ->
      let (typetyvars, typequalvars) = type_free_vars t in
      let (envtypvars, envqualvars) = env_free_vars es in
	(typetyvars @ envtypvars, typequalvars @ envqualvars)


let generalize_type t tenv =
  let rec generalize_rec t tyvars qualvars =
    match (tyvars, qualvars) with
	([], []) ->
	  t
      | (vs, q::qs) ->
	  let alias = getfreshqualvar() in
	    begin match alias with
		QualVar k' ->
		  ForallQual(k', QualVar q, typ_subst_qualvar alias q (generalize_rec t vs qs))
	      | _ ->
		  raise Unify
	    end
      | (v::vs, []) ->
	  ForallTyp(v, generalize_rec t vs [])
  in
  let (tyfree, qualfree) = type_free_vars t in
  let (envtyfree, envqualfree) = env_free_vars tenv in
  let generictyvars = set_minus tyfree envtyfree in
  let genericqualvars = set_minus qualfree envqualfree in
    generalize_rec t generictyvars genericqualvars


let rec instantiate_type = function
    ForallTyp(a, t) ->
      let (ty, constrs) = instantiate_type t in
	(typ_subst_tyvar (getfreshvar()) a ty, constrs)
  | ForallQual(k, q, t) ->
      let (ty, constrs) = instantiate_type t in
      let newqualvar = getfreshqualvar() in
	(typ_subst_qualvar newqualvar k ty,
	 (QualLessEq(newqualvar, q))::constrs)
  | t ->
      (t, [])


let rec type_lub t1 t2 =
  let rec type_merge_rec t1 t2 meet join =
    let toplevel = join (get_qualifier t1) (get_qualifier t2) in
      match (t1, t2) with
	  (Arrow(_, t1, t1'), Arrow(_, t2, t2')) ->
	    let t =
	      begin match (t1, t2) with
		  (Arrow _, Arrow _) ->
		    type_merge_rec t1 t2 join meet
		| _ ->
		    let (q1, q2) = (get_qualifier t1, get_qualifier t2) in
		      (* Qualifiers on the LHS of an arrow must be either literals
			 (and their combinations) or singular qualifier variables.
			 Unification will ensure the LHS remains the same in the latter
			 case, so we don't worry about it here.
		      *)
		      match (q1, q2) with
			  (QualVar _, QualVar _) ->
			    t1
			| _ ->
			    set_qualifier t1 (meet q1 q2)
	      end
	    in
	    let t' = type_merge_rec t1' t2' meet join in
	      Arrow(toplevel, t, t')
	| _ ->
	    set_qualifier t1 toplevel
  and qmeet q1 q2 =
    QualMeet(q1, q2)
  and qjoin q1 q2 =
    QualJoin(q1, q2)
  in
    type_merge_rec t1 t2 qmeet qjoin


let infer_type e =
  let rec infer_rec e tenv =
    match e with
	Num(_) ->
	  (Int(Top), [])
      | True
      | False ->
	  (Bool(Top), [])
      | Var(x) ->
	  let tx' = env_lookup x tenv in
	  let (tx, constrs) = instantiate_type tx' in
	    (tx, constrs)
      | Annot(q, e) ->
	  let (t, constrs) = infer_rec e tenv in
	    (annotate_type t q, constrs)
      | If(c, e1, e2) ->
	  let (tc, constrsc) = infer_rec c tenv in
	  let (t1, constrs1) = infer_rec e1 tenv in
	  let (t2, constrs2) = infer_rec e2 tenv in
	  let t = type_lub t1 t2 in
	    (t,
	     TypEq(t1, t2)::TypEq(tc, Bool(getfreshqualvar()))::(constrsc @ constrs1 @ constrs2))
      | App(e1, e2) ->
	  let (t1, constrs1) = infer_rec e1 tenv in
	  let (t2, constrs2) = infer_rec e2 tenv in
	    begin match t1 with
		Arrow(q, _, returnty) ->
		  let funty = Arrow(q, t2, returnty) in
		    (returnty, TypEq(t1, funty)::(constrs1 @ constrs2))
	      | _ ->
		  raise Unify
	    end
      | Abs(x, _, e) ->
	  let tx = getfreshvar() in
	  let newtenv = env_add x tx tenv in
	  let (t', constrs) = infer_rec e newtenv in
	    begin match tx with
		TyVar(q, _) ->
		  (Arrow(Top, tx, t'), QualLessEq(q, Top)::constrs)
	      | _ ->
		  raise Unify
	    end
      | Let(x, _, ex, e) ->
	  let (tx, constrsx) = infer_rec ex tenv in
	  let tx = generalize_type tx tenv in
	  let newtenv = env_add x tx tenv in
	  let (te, constrse) = infer_rec e newtenv in
	    (te, constrse @ constrsx)
      | _ ->
	  raise Unify
  in
  let (t, constrs) = infer_rec e [] in
  let sub = unify constrs in
    sub t


let rec pprint_type t =
  let rec pprint_qual = function
      Top -> "top"
    | Bottom -> "bottom"
    | Qual q -> q
    | QualVar k -> sprintf "'%s" k
    | QualMeet(q1, q2) -> sprintf "%s & %s" (pprint_qual q1) (pprint_qual q2)
    | QualJoin(q1, q2) -> sprintf "%s | %s" (pprint_qual q1) (pprint_qual q2)
  in
    match t with
	Arrow(q, t1, t2) -> sprintf "%s (%s -> %s)" (pprint_qual q) (pprint_type t1) (pprint_type t2)
      | Int(q) -> sprintf "%s int" (pprint_qual q)
      | Bool(q) -> sprintf "%s bool" (pprint_qual q)
      | TyVar(q, x) -> sprintf "%s '%s" (pprint_qual q) x
      | Nil -> "[nil]"
      | ForallQual(k, q, t) -> sprintf "'%s <= %s. %s" k (pprint_qual q) (pprint_type t)
      | ForallTyp(a, t) -> sprintf "'%s. %s" a (pprint_type t)

