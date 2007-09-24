type binop =
    Plus
  | Minus
  | Times

type binrel =
    Eq
  | Ne
  | Lt
  | Le 

type pexpr =
    PInt of int 
  | Var of Ident.t
  | Pvar of Ident.t * int
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr 

type predicate =
    True
  | Atom of pexpr * binrel * pexpr 
  | Not of predicate
  | And of predicate * predicate 
  | Or of predicate * predicate

let equals(p, q) =
  Atom(p, Eq, q)

let big_and cs =
  List.fold_right (fun p q -> And(p, q)) cs True

let big_or cs =
  List.fold_right (fun p q -> Or(p, q)) cs (Not(True))

let rec predexpr_subst v x = function
    Var y when Ident.same y x ->
      v
  | FunApp(f, e) ->
      FunApp(f, predexpr_subst v x e)
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

let predicate_vars p =
  let rec exp_vars_rec vars = function
      PInt _ ->
        vars
    | Var x ->
        x::vars
    | Pvar(x, _) ->
        x::vars
    | FunApp(_, e) ->
        exp_vars_rec vars e
    | Binop(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
  in
  let rec vars_rec vars = function
      True ->
        vars
    | Atom(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
    | Not p ->
        vars_rec vars p
    | And(p1, p2)
    | Or(p1, p2) ->
        let vars' = vars_rec vars p1 in
          vars_rec vars' p2
  in
    vars_rec [] p

let implies(p, q) =
  Or(Not p, q)
(*
let fresh_pexprvar = Misc.make_get_fresh (fun x -> Var ("__" ^ x))
*)
(*
let rec transl_predicate p =
  let transl_op = function
      Predexp_plus -> Plus
    | Predexp_minus -> Minus
    | Predexp_times -> Times
  in
  let rec transl_pexpression pexp =
    match pexp.ppredexp_desc with
	Ppredexp_int (n) ->
	  PInt n
      | Ppredexp_var (y) ->
	  Var y
      | Ppredexp_pvar (y, n) ->
	  Pvar (y, n)
      | Ppredexp_app (f, e) ->
	  FunApp (f, parse_pexpression e)
      | Ppredexp_binop (e1, op, e2) ->
	  Binop (parse_pexpression e1, parse_op op, parse_pexpression e2)
  in
  let transl_rel = function
      Pred_eq -> Eq
    | Pred_ne -> Ne
    | Pred_lt -> Lt
    | Pred_le -> Le
  in
  let rec transl_pred_rec pred =
    match pred.ppred_desc with
	Ppred_true ->
	  True
      | Ppred_atom (e1, rel, e2) ->
	  Atom (parse_pexpression e1, parse_rel rel, parse_pexpression e2)
      | Ppred_not (p) ->
	  Not (parse_predicate p)
      | Ppred_and (p1, p2) ->
	  And (parse_predicate p1, parse_predicate p2)
      | Ppred_or (p1, p2) ->
	  Or (parse_predicate p1, parse_predicate p2)
  in parse_pred_rec p

let pprint_binop frec e1 op e2 =
  let opstr =
    match op with
	Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
  in
  let (str1, str2) = (frec e1, frec e2) in
    Misc.join [str1; opstr; str2] " "

let pprint_binrel frec e1 rel e2 =
  let relstr =
    match rel with
	Eq -> "="
      | Ne -> "!="
      | Lt -> "<"
      | Le -> "<="
  in
  let (str1, str2) = (frec e1, frec e2) in
    Misc.join [str1; relstr; str2] " "

let rec pprint_pexpr = function
    PInt(n) ->
      string_of_int n
  | Var x ->
      x
  | Pvar(x, n) ->
      x ^ "_" ^ (string_of_int n)
  | FunApp(f, e) ->
      Printf.sprintf "%s(%s)" f (pprint_pexpr e)
  | Binop(e1, op, e2) ->
      pprint_binop pprint_pexpr e1 op e2

let rec pprint_predicate = function
    True ->
      "true"
  | Atom(e1, rel, e2) ->
      "(" ^ pprint_binrel pprint_pexpr e1 rel e2 ^ ")"
  | Not(p) ->
      "(not " ^ pprint_predicate p ^ ")"
  | And(p, q) ->
      "(and " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
  | Or(p, q) ->
      "(or " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
*)
