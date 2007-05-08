open Expr
open Flowgraph


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


type parameterized_pred = PredOver of (string * predicate)
type named_pred = string * parameterized_pred


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
  Var("v" ^ (expr_get_id e))


let branch_var e =
  Var("b" ^ (expr_get_id e))


exception NoExpression

(* pmr: is this the right place for this? *)
let vertex_value_expression v =
  let varname =
    match FlowGraph.V.label v with
	ExprId(id) ->
	  "v" ^ id
      | VarName(x) ->
	  x
      | NonExpr(_) ->
	  raise NoExpression
  in
    Var(varname)


let vertex_branch_expression v =
  match FlowGraph.V.label v with
      ExprId(id) ->
	Var("b" ^ id)
    | VarName(x) ->
	Int(1)
    | NonExpr(_) ->
	raise NoExpression


let qualmap_to_predicates qm preds =
  let make_qualifier_pred be ve q =
    match List.assoc q preds with
	PredOver(x, p) ->
	  Or(Not(Atom(be, Eq, Int(1))), predicate_subst ve x p)
  in
  let definite_quals_to_predicates = function
      (v, qs) ->
	try
	  let be = vertex_branch_expression v in
	  let ve = vertex_value_expression v in
	    List.map (make_qualifier_pred be ve) qs
	with NoExpression ->
	  []
  in
  let definite_quals = qualmap_definite_quals qm in
    List.flatten (List.map definite_quals_to_predicates definite_quals)


let equals(p, q) =
  Atom(p, Eq, q)


let big_and cs =
  let combine p q = And(p, q) in
    List.fold_right combine cs True


let big_or cs =
  let combine p q = And(p, q) in
    List.fold_right combine cs (Not(True))


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
      | TyCon(_, exps, _) ->
	  big_and (List.map value_predicate exps)
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
	  value_predicate e
      | App(e1, e2, _) ->
	  big_and (List.map value_predicate [e1; e2])
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
      | TyCon(_, exps, _) ->
	  let child_bps = List.map branch_predicate exps in
	  let bs = List.map branch_active exps in
	    big_and ([implies(big_or bs, be); implies(be, big_and bs)]@child_bps)	    
      | BinOp(_, e1, e2, _)
      | BinRel(_, e1, e2, _)
      | Let(_, _, e1, e2, _)
      | App(e1, e2, _) ->
	  let child_bps = List.map branch_predicate [e1; e2] in
	  let (b1, b2) = (branch_active e1, branch_active e2) in
	    big_and ([implies(Or(b1, b2), be); implies(be, And(b1, b2))]@child_bps)
      | If(e1, e2, e3, _) ->
	  let child_bps = List.map branch_predicate [e1; e2; e3] in
	  let v1 = value_var e1 in
	  let (b1, b2, b3) = (branch_active e1, branch_active e2, branch_active e3) in
	    big_and ([implies(b1, be);
		      implies(b2, And(equals(v1, Int 1), be));
		      implies(b3, And(equals(v1, Int 0), be));
		      implies(be, b1);
		      implies(be, Or(b2, b3))]@child_bps)
      | Abs(_, _, e, _) ->
	  big_and [implies(branch_active e, be); branch_predicate e]
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
      "(" ^ pprint_binrel pprint_expression e1 rel e2 ^ ")"
  | Not(p) ->
      "(not " ^ pprint_predicate p ^ ")"
  | And(p, q) ->
      "(and " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
  | Or(p, q) ->
      "(or " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
