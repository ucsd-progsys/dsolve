open Expr


type expression =
    Int of int 
  | Var of string
  | Pvar of string * int
  | Binop of expression * binop * expression 


type predicate =
    True
  | Atom of expression * binrel * expression 
  | Not of predicate
  | And of predicate * predicate 
  | Or of predicate * predicate


type parameterized_pred = PredOver of string * string * predicate


let rec predexpr_subst v x = function
    Var y when y = x ->
      v
  | Binop(e1, op, e2) ->
      Binop(predexpr_subst v x e1, op, predexpr_subst v x e2)
  | e ->
      e


let rec predicate_subst v x = function
    True ->
      True
  | Atom(e1, rel, e2) ->
      Atom(predexpr_subst v x e1, rel, predexpr_subst v x e2)
  | Not(p) ->
      Not(predicate_subst v x p)
  | And(p, q) ->
      And(predicate_subst v x p, predicate_subst v x q)
  | Or(p, q) ->
      Or(predicate_subst v x p, predicate_subst v x q)


let value_var e =
  Var("v" ^ (string_of_int (expr_get_id e)))


let branch_var e =
  Var("b" ^ (string_of_int (expr_get_id e)))


let equals(p, q) =
  Atom(p, Eq, q)


let big_and cs =
  let combine p q = And(p, q) in
    List.fold_right combine cs True


exception NoPredicate


let rec value_predicate e =
  let ve = value_var e in
    match e with
	Num(n, _) ->
	  equals(ve, Int n)
      | TrueExp(_) ->
	  equals(ve, Int 1)
      | FalseExp(_) ->
	  equals(ve, Int 0)
      | ExpVar(x, _) ->
	  equals(ve, Var x)
      | BinOp(op, e1, e2, _) ->
	  let child_vps = List.map value_predicate [e1; e2] in
	  let (v1, v2) = (value_var e1, value_var e2) in
	    big_and ((equals(ve, Binop(v1, op, v2)))::child_vps)
      | BinRel(rel, e1, e2, _) ->
	  let child_vps = List.map value_predicate [e1; e2] in
	  let (v1, v2) = (value_var e1, value_var e2) in
	  let relp = Atom(v1, rel, v2) in
	    big_and ((Or(And(equals(ve, Int 1), relp),
                         And(equals(ve, Int 0), Not(relp))))::child_vps)
      | If(e1, e2, e3, _) ->
	  let child_vps = List.map value_predicate [e1; e2; e3] in
	  let (v1, v2, v3) = (value_var e1, value_var e2, value_var e3) in
	  let true_branch = And(equals(v1, Int 1), equals(ve, v2)) in
	  let false_branch = And(equals(v1, Int 0), equals(ve, v3)) in
	    big_and ((Or(true_branch, false_branch))::child_vps)
      | Let(x, _, e1, e2, _) ->
	  let child_vps = List.map value_predicate [e1; e2] in
	  let (v1, v2) = (value_var e1, value_var e2) in
	    big_and ([equals(Var x, v1); equals(ve, v2)]@child_vps)
      | Abs(x, _, e, _) ->
	  let child_vp = value_predicate e in
	    child_vp
      | _ ->
	  raise NoPredicate


let implies(p, q) =
  Or(Not p, q)


let branch_active(e) =
  Atom(branch_var e, Eq, Int 1)


let rec branch_predicate e =
  let be = branch_active e in
    match e with
	Num(_, _)
      | TrueExp(_)
      | FalseExp(_)
      | ExpVar(_, _) ->
	  True
      | BinOp(_, e1, e2, _)
      | BinRel(_, e1, e2, _)
      | Let(_, _, e1, e2, _) ->
	  let child_bps = List.map branch_predicate [e1; e2] in
	  let (b1, b2) = (branch_active e1, branch_active e2) in
	    big_and ([implies(b1, be); implies(b2, be)]@child_bps)
      | If(e1, e2, e3, _) ->
	  let child_bps = List.map branch_predicate [e1; e2; e3] in
	  let v1 = value_var e1 in
	  let (b1, b2, b3) = (branch_active e1, branch_active e2, branch_active e3) in
	    big_and ([implies(b1, be);
		      implies(b2, And(equals(v1, Int 1), be));
		      implies(b3, And(equals(v1, Int 0), be))]@child_bps)
      | Abs(_, _, e, _) ->
	  let child_bp = branch_predicate e in
	    big_and [implies(branch_active e, be); child_bp]
      | _ ->
	  raise NoPredicate


let expr_predicate e =
  And(value_predicate e, branch_predicate e)


let rec pprint_expression = function
    Int(n) ->
      string_of_int n
  | Var x ->
      x
  | Pvar(x, n) ->
      x ^ "_" ^ (string_of_int n)
  | Binop(e1, op, e2) ->
      pprint_binop pprint_expression e1 op e2


let rec pprint_predicate = function
    True ->
      "true"
  | Atom(e1, rel, e2) ->
      pprint_binrel pprint_expression e1 rel e2
  | Not(p) ->
      "(not " ^ pprint_predicate p ^ ")"
  | And(p, q) ->
      "(" ^ pprint_predicate p ^ " and " ^ pprint_predicate q ^ ")"
  | Or(p, q) ->
      "(" ^ pprint_predicate p ^ " or " ^ pprint_predicate q ^ ")"
