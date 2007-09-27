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

let rec pexp_map_vars f pexp =
  let rec map_rec = function
      Var y ->
        f y
    | FunApp (fn, e) ->
        FunApp (fn, map_rec e)
    | Binop (e1, op, e2) ->
        Binop (map_rec e1, op, map_rec e2)
    | e ->
        e
  in map_rec pexp

let rec map_vars f pred =
  let rec map_rec = function
      True ->
        True
    | Atom (e1, rel, e2) ->
        Atom (pexp_map_vars f e1, rel, pexp_map_vars f e2)
    | Not p ->
        Not (map_rec p)
    | And (p, q) ->
        And (map_rec p, map_rec q)
  | Or (p, q) ->
      Or (map_rec p, map_rec q)
  in map_rec pred

let predicate_subst v x pred =
  map_vars (fun y -> if Ident.same x y then v else Var y) pred

let rec instantiate_named_vars subs pred =
  map_vars (fun y -> Var (List.assoc (Ident.name y) subs)) pred

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
*)
let pprint_binop frec e1 op e2 =
  let opstr =
    match op with
	Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
  in
  let (str1, str2) = (frec e1, frec e2) in
    String.concat " " [str1; opstr; str2]

let pprint_binrel frec e1 rel e2 =
  let relstr =
    match rel with
	Eq -> "="
      | Ne -> "!="
      | Lt -> "<"
      | Le -> "<="
  in
  let (str1, str2) = (frec e1, frec e2) in
    String.concat " " [str1; relstr; str2]

let rec pprint_pexpr = function
    PInt(n) ->
      string_of_int n
  | Var x ->
      Ident.unique_name x
  | Pvar(x, n) ->
      (Ident.unique_name x) ^ "_" ^ (string_of_int n)
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

