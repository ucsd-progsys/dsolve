open Env
open Expr
open Printf


exception TypeMismatch of expr * typschema
exception BadCoercion


(* Some convenient shorthands *)
let qs s = QSchema s
let mt m = qs (MonoTyp m)
let ql l = QualLiteral l


(* Type manipulation *)
let monotyp_from_typschema t =
  match t with
      QSchema(MonoTyp(t)) ->
	t
    | _ -> raise BadCoercion
and qschema_from_typschema t =
  match t with
      QSchema(t) ->
	t
    | _ -> raise BadCoercion


let annotate_type q t =
  let rec annotate_typschema = function
      ForallTyp(a, t) ->
	ForallTyp(a, annotate_typschema t)
    | QSchema(t) ->
	QSchema(annotate_qualschema t)
  and annotate_qualschema = function
      ForallQual(k, q', t) ->
	ForallQual(k, q', annotate_qualschema t)
    | MonoTyp(t) ->
	MonoTyp(annotate_monotyp t)
  and annotate_monotyp = function
      Arrow(q', t1, t2) ->
	Arrow(QualMeet(q, q'), t1, t2)
    | Int(q') ->
	Int(QualMeet(q, q'))
    | Bool(q') ->
	Bool(QualMeet(q, q'))
    | TyVar(q', a) ->
	TyVar(QualMeet(q, q'), a)
    | Nil ->
	Nil
  in
    annotate_typschema t


let subst_qualvar q k t =
  let rec subst_q_typschema = function
      ForallTyp(a, t) ->
	ForallTyp(a, subst_q_typschema t)
    | QSchema(t) ->
	QSchema(subst_q_qualschema t)
  and subst_q_qualschema = function
      ForallQual(k', b, t) when k <> k' ->
	ForallQual(k', b, subst_q_qualschema t)
    | MonoTyp(t) ->
	MonoTyp(subst_q_monotyp t)
    | t ->
	t
  and subst_q_monotyp = function
      Arrow(q', t, t') ->
	Arrow(subst_q_qual q', subst_q_monotyp t, subst_q_monotyp t')
    | Int(q) ->
	Int(subst_q_qual q)
    | Bool(q) ->
	Bool(subst_q_qual q)
    | TyVar(q, a) ->
	TyVar(subst_q_qual q, a)
    | Nil ->
	Nil
  and subst_q_qual = function
    | QualVar(k') when k = k' ->
	q
    | QualMeet(q1, q2) ->
	QualMeet(subst_q_qual q1, subst_q_qual q2)
    | QualJoin(q1, q2) ->
	QualJoin(subst_q_qual q1, subst_q_qual q2)
    | q ->
	q
  in
    subst_q_typschema t


let subst_tyvar b a t =
  let rec subst_ty_typschema = function
      ForallTyp(a', t) when a <> a' ->
	ForallTyp(a', subst_ty_typschema t)
    | QSchema(t) ->
	QSchema(subst_ty_qualschema t)
    | t ->
	t
  and subst_ty_qualschema = function
      ForallQual(k, q, t) ->
	ForallQual(k, q, subst_ty_qualschema t)
    | MonoTyp(t) ->
	MonoTyp(subst_ty_monotyp t)
  and subst_ty_monotyp = function
      Arrow(q, t1, t2) ->
	Arrow(q, subst_ty_monotyp t1, subst_ty_monotyp t2)
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
  in
    subst_ty_typschema t


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
    | (_, QualLiteral(Top)) ->
	true
    | (QualLiteral(Bottom), _) ->
	true
    | (QualLiteral(q1), QualLiteral(q2)) ->
	q1 = q2
    | _ ->
	false


(* Type checking *)
let subtype t1 t2 =
  let rec subtype_mono t1 t2 =
    match (t1, t2) with
	(Arrow(q1, t1, t1'), Arrow(q2, t2, t2')) ->
	  qualifier_less_equal q1 q2 &&
	    subtype_mono t1' t2' &&
	    subtype_mono t2 t1
      | (Int(q1), Int(q2)) ->
	  qualifier_less_equal q1 q2
      | (Bool(q1), Bool(q2)) ->
	  qualifier_less_equal q1 q2
      | (TyVar(q1, s1), TyVar(q2, s2)) when s1 = s2 ->
	  qualifier_less_equal q1 q2
      | _ ->
	  false
  and subtype_qualschema t1 t2 =
    match (t1, t2) with
	(ForallQual(k1, q1, t1), ForallQual(k2, q2, t2)) ->
	  if (qualifier_less_equal (ql q2) (ql q1)) then
	    let t1 = qschema_from_typschema (subst_qualvar (ql q2) k1 (qs t1)) in
	    let t2 = qschema_from_typschema (subst_qualvar (ql q2) k2 (qs t2)) in
	      subtype_qualschema t1 t2
	  else
	    false
      | (MonoTyp(t1), MonoTyp(t2)) ->
	  subtype_mono t1 t2
      | _ ->
	  false
  and subtype_typschema t1 t2 =
    match (t1, t2) with
	(ForallTyp(a1, t1), ForallTyp(a2, t2)) ->
	  let t2 = subst_tyvar (TyVar(ql Top, a1)) a2 t2 in
	    subtype_typschema t1 t2
      | (QSchema(t1), QSchema(t2)) ->
	  subtype_qualschema t1 t2
      | _ ->
	  false
  in
    subtype_typschema t1 t2


let qualifiers_equal q1 q2 =
  qualifier_less_equal q1 q2 &&
    qualifier_less_equal q2 q1


(* While it would be nice to do types_equal analogously to qualifiers_equal, it
   doesn't work; the subtype relation is not antisymmetric.  (Is this a
   deficiency of the subtype relation?) *)
let types_equal t1 t2 =
  let rec typschemas_equal t1 t2 =
    match (t1, t2) with
	(ForallTyp(a1, t1), ForallTyp(a2, t2)) ->
	  let t2 = subst_tyvar (TyVar(ql Top, a1)) a2 t2 in
	    typschemas_equal t1 t2
      | (QSchema(t1), QSchema(t2)) ->
	  qualschemas_equal t1 t2
      | _ ->
	  false
  and qualschemas_equal t1 t2 =
    match (t1, t2) with
	(ForallQual(k1, q1, t1), ForallQual(k2, q2, t2)) ->
	  let t2 = qschema_from_typschema (subst_qualvar (QualVar k1) k2 (QSchema t2)) in
	    qualifiers_equal (ql q1) (ql q2) &&
	      qualschemas_equal t1 t2
      | (MonoTyp(t1), MonoTyp(t2)) ->
	  monotyps_equal t1 t2
      | _ ->
	  false
  and monotyps_equal t1 t2 =
    match (t1, t2) with
	(Arrow(q1, t1, t1'), Arrow(q2, t2, t2')) ->
	  qualifiers_equal q1 q2 && monotyps_equal t1 t2 && monotyps_equal t1' t2'
      | (Int(q1), Int(q2))
      | (Bool(q1), Bool(q2)) ->
	  qualifiers_equal q1 q2
      | (TyVar(q1, a1), TyVar(q2, a2)) when a1 = a2 ->
	  qualifiers_equal q1 q2
      | _ ->
	  false
  in
    typschemas_equal t1 t2



let check_type e texp =
  let rec type_exp e texp tenv =
  match e with
      Num(_) ->
	mt (Int(ql Top))
    | True
    | False ->
	mt (Bool(ql Top))
    | Var(x) ->
	env_lookup x tenv
    | Let(x, tx, ex, e) ->
	let tx' = type_exp ex tx tenv in
	  if subtype tx' tx then
	    let newtenv = env_add x tx tenv in
	      type_exp e texp newtenv
	  else
	    raise (TypeMismatch(ex, tx))
    | Abs(x, tx, e) ->
	begin match monotyp_from_typschema texp with
	    Arrow(_, _, returnexp) ->
	      let newtenv = env_add x (mt tx) tenv in
	      let t' = monotyp_from_typschema (type_exp e (mt returnexp) newtenv) in
		mt (Arrow(ql Top, tx, t'))
	  | _ ->
		raise (TypeMismatch(e, texp))
	  end
    | App(e1, e2) ->
	(* Fake an expected type for the function being applied -
	   all we're really interested in is a proper return type
	   (see above) *)
	let expected_t1 =
	  mt (Arrow(ql Top, TyVar(QualVar("k"), "a"), monotyp_from_typschema texp))
	in
	  begin match monotyp_from_typschema (type_exp e1 expected_t1 tenv) with
	      Arrow(q, t, t') ->
		let t2 = type_exp e2 (mt t) tenv in
		  if subtype t2 (mt t) then
		    mt t'
		  else
		    raise (TypeMismatch(e2, mt t'))
	    | t ->
		raise (TypeMismatch(e1, mt t))
	  end
    | QualAbs(k, q, e) ->
	begin match qschema_from_typschema texp with
	    ForallQual(_, q', t) ->
	      if qualifiers_equal (ql q) (ql q') then
		let t' = qschema_from_typschema (type_exp e (qs t) tenv) in
		  qs (ForallQual(k, q, t'))
	      else
		raise (TypeMismatch(e, texp))
	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    | QualApp(e, q) ->
	(* If we were calculating the type for this thing afresh, we'd need a precise
	   expected type.  But since this has to appear in the body of a let, we're
	   only deconstructing an existing type at this point and we're ok. *)
	begin match qschema_from_typschema (type_exp e (mt Nil) tenv) with
	    ForallQual(k, q', t) ->
	      if qualifier_less_equal (ql q) (ql q') then
		match subst_qualvar (ql q) k (qs t) with
		    QSchema(t) ->
		      qs t
		  | _ ->
		      raise (TypeMismatch(e, texp))
	      else
		raise (TypeMismatch(e, texp))
	  | _ ->
	      raise (TypeMismatch(e, texp))
	end
    |	Annot(q, e) ->
	  (* XXX: *sniff* smells like...  bogitude - but we don't know what type's being annotated,
	     so I have no idea what can be done about removing the annotation *)
	  let t = type_exp e texp tenv in
	    annotate_type q t
    | If(c, e1, e2) ->
	begin match monotyp_from_typschema (type_exp c (mt (Bool(ql Top))) tenv) with
	    Bool(QualLiteral(Top)) ->
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
	begin match (type_exp e (mt Nil) tenv) with
	    ForallTyp(a, s) ->
	      subst_tyvar t a s
	  | t ->
	      raise (TypeMismatch(e, t))
	end
  in
    try
      let t = type_exp e texp [] in
	types_equal t texp
    with _ ->
      false


exception Unify


(* XXX: the following are incomplete, to say the least *)

let unify t1 t2 meet join =
  if types_equal t1 t2 then
    t1
  else
    raise Unify


let type_meet t1 t2 = t1


let type_join t1 t2 = t1


let infer_type e =
  let rec infer_rec e tenv =
    match e with
	Num(_) ->
	  mt (Int(ql Top))
      | True
      | False ->
	  mt (Bool(ql Top))
      | Var(x) ->
	  env_lookup x tenv
      | Annot(q, e) ->
	  annotate_type q (infer_rec e tenv)
      | If(c, e1, e2) ->
	  begin match monotyp_from_typschema(infer_rec c tenv) with
	      Bool(_) ->
		let t1 = infer_rec e1 tenv in
		let t2 = infer_rec e2 tenv in
		  unify t1 t2 type_meet type_join
	    | _ ->
		raise Unify
	  end
      | App(e1, e2) ->
	  let t1 = infer_rec e1 tenv in
	  let t2 = infer_rec e2 tenv in
	    begin match monotyp_from_typschema(t1) with
		Arrow(_, t, t') ->
		  if subtype t2 (mt t) then
		    mt t'
		  else
		    raise Unify
	      | _ ->
		  raise Unify
	    end
      | _ ->
	  raise Unify
  in
    infer_rec e []


let pprint_type t =
  let rec pprint_qualliteral = function
      Top -> "top"
    | Bottom -> "bottom"
    | Qual q -> q
  and pprint_qual = function
      QualVar k -> sprintf "'%s" k
    | QualMeet(q1, q2) -> sprintf "%s & %s" (pprint_qual q1) (pprint_qual q2)
    | QualJoin(q1, q2) -> sprintf "%s | %s" (pprint_qual q1) (pprint_qual q2)
    | QualLiteral q -> pprint_qualliteral q
  and pprint_monotyp = function
      Arrow(q, t1, t2) -> sprintf "%s (%s -> %s)" (pprint_qual q) (pprint_monotyp t1) (pprint_monotyp t2)
    | Int(q) -> sprintf "%s int" (pprint_qual q)
    | Bool(q) -> sprintf "%s bool" (pprint_qual q)
    | TyVar(q, x) -> sprintf "%s '%s" (pprint_qual q) x
    | Nil -> "[nil]"
  and pprint_qualschema = function
      ForallQual(k, q, t) -> sprintf "'%s <= %s. %s" k (pprint_qualliteral q) (pprint_qualschema t)
    | MonoTyp(t) -> pprint_monotyp t
  and pprint_typschema = function
      ForallTyp(a, t) -> sprintf "'%s. %s" a (pprint_typschema t)
    | QSchema(t) -> pprint_qualschema t
  in
    pprint_typschema t
	
      
