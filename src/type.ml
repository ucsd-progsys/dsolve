open Printf
open Predicate
open TheoremProver


(* Type language *)
type qualifier = string * parameterized_pred


type typ =
    Arrow of string * typ * typ
  | Int of qualifier list
  | TyVar of string


let rec typ_subst_tyvar b a = function
  | Arrow(x, t1, t2) ->
      Arrow(x, typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)
  | TyVar a' when a' = a ->
      b
  | t ->
      t


let const_int_quals quals guard n =
  let n_exp = PInt n in
  let n_satisfies (_, PredOver(x, p)) =
    Prover.implies guard (predicate_subst n_exp x p)
  in
    List.filter n_satisfies quals


(*
let check_type e texp =
  let rec type_exp e texp tenv =
    let type_int_op e1 e2 rtype =
      begin match (type_exp e1 (Int(Top)) tenv, type_exp e2 (Int(Top)) tenv) with
	  (Int(_), Int(_)) ->
	    rtype
	| (Int(_), _) ->
	    raise (TypeMismatch(e1, Int(Top)))
	| _ ->
	    raise (TypeMismatch(e2, Int(Top)))
      end
    in
      match e with
	  Num(_, _) ->
	    Int(Top)
	| TrueExp(_)
	| FalseExp(_) ->
	    Bool(Top)
	| ExpVar(x, _) ->
	    env_lookup x tenv
	| Let(x, tx, ex, e, _) ->
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
	| Abs(x, tx, e, _) ->
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
	| App(e1, e2, _) ->
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
	| QualAbs(k, q, e, _) ->
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
	| QualApp(e, q, _) ->
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
	| Annot(q, e, _) ->
	    (* XXX: *sniff* smells like...  bogitude - but we don't know what type's being annotated,
	       so I have no idea what can be done about removing the annotation *)
	    let t = type_exp e texp tenv in
	      annotate_type t q
	| BinOp(_, e1, e2, _) ->
	    type_int_op e1 e2 (Int(Top))
	| BinRel(r, e1, e2, _) ->
	    begin match r with
		Lt
	      | Le ->
		  type_int_op e1 e2 (Bool(Top))
	      | Eq
	      | Ne ->
		  let (t1, t2) = (type_exp e1 Nil tenv, type_exp e2 Nil tenv) in
		    begin match (t1, t2) with
			(Int(_), Int(_))
		      | (Bool(_), Bool(_)) ->
			  Bool(Top)
		      | _ ->
			  raise (TypeMismatch(e, t1))
		    end
	    end
	| If(c, e1, e2, _) ->
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
	| TyAbs(a, e, _) ->
	    begin match texp with
		ForallTyp(_, t) ->
		  ForallTyp(a, type_exp e t tenv)
	      | _ ->
		  raise (TypeMismatch(e, texp))
	    end
	| TyApp(e, t, _) ->
	    (* We can get away with faking the expected type for the same reasons it's ok for
	       QualApp *)
	    begin match (type_exp e Nil tenv) with
		ForallTyp(a, s) ->
		  typ_subst_tyvar t a s
	      | t ->
		  raise (TypeMismatch(e, t))
	    end
	| _ ->
	    (* pmr: want to handle fix here *)
	    raise Not_found
  in
    try
      let t = type_exp e texp [] in
	types_equal t texp
    with _ ->
      false
*)


let rec pprint_type = function
    Arrow(x, (Arrow _ as t1), t2) ->
      sprintf "%s: (%s) -> %s" x (pprint_type t1) (pprint_type t2)
  | Arrow(x, t1, t2) ->
      sprintf "%s: %s -> %s" x (pprint_type t1) (pprint_type t2)
  | Int quals ->
      let qual_name (name, _) = name in
      let qual_names = Misc.join (List.map qual_name quals) " " in
      sprintf "%s int" qual_names
  | TyVar a ->
      sprintf "'%s" a
