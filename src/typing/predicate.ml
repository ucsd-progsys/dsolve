open Parsetree
open Asttypes
open Format

type binop =
    Plus
  | Minus
  | Times
  | Div

type binrel =
    Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le 

type pexpr =
    PInt of int 
  | Var of Path.t
  | Pvar of Path.t * int
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr 

type t =
    True
  | Atom of pexpr * binrel * pexpr 
  | Not of t
  | And of t * t 
  | Or of t * t

let pprint_rel = function
    Eq -> "="
  | Ne -> "=/="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let rec pprint_pexpr ppf = function
  | PInt n ->
      fprintf ppf "%d" n
  | Var id ->
      fprintf ppf "%s" (Path.unique_name id)
  | Pvar (id, n) ->
      fprintf ppf "%s-%d" (Path.unique_name id) n
  | FunApp (f, pexp) ->
      fprintf ppf "@[%s@ %a@]" f pprint_pexpr pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
      in fprintf ppf "@[%a@ %s@ %a@]" pprint_pexpr p opstr pprint_pexpr q

let rec pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[%a@ %s@ %a@]" pprint_pexpr p (pprint_rel rel) pprint_pexpr q
  | Not p ->
      fprintf ppf "@[not@ %a@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[%a@ and@ %a@]" pprint p pprint q
  | Or (p, q) ->
      fprintf ppf "@[%a@ or@ %a@]" pprint p pprint q


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

let subst v x pred =
  map_vars (fun y -> if Path.same x y then v else Var y) pred

let rec instantiate_named_vars subs pred =
  map_vars (fun y -> Var (List.assoc (Path.name y) subs)) pred

let vars p =
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

let rec transl_predicate p =
  let transl_op = function
    | Predexp_plus -> Plus
    | Predexp_minus -> Minus
    | Predexp_times -> Times
		| Predexp_div -> Div
  in
  let rec transl_pexpression pexp =
    match pexp.ppredexp_desc with
      | Ppredexp_int n ->
	  PInt n
      | Ppredexp_var y ->
	  Var (Path.mk_ident y)
      | Ppredexp_pvar (y, n) ->
	  Pvar (Path.mk_ident y, n)
      | Ppredexp_app (f, e) ->
	  FunApp (f, transl_pexpression e)
      | Ppredexp_binop (e1, op, e2) ->
	  Binop (transl_pexpression e1, transl_op op, transl_pexpression e2)
  in
  let transl_rel = function
    | Pred_eq -> Eq
    | Pred_ne -> Ne
    | Pred_gt -> Gt
    | Pred_ge -> Ge
    | Pred_lt -> Lt
    | Pred_le -> Le
  in
  let rec transl_pred_rec pred =
    match pred.ppred_desc with
      | Ppred_true ->
	  True
      | Ppred_atom (e1, rel, e2) ->
	  Atom (transl_pexpression e1, transl_rel rel, transl_pexpression e2)
      | Ppred_not p ->
	  Not (transl_pred_rec p)
      | Ppred_and (p1, p2) ->
	  And (transl_pred_rec p1, transl_pred_rec p2)
      | Ppred_or (p1, p2) ->
	  Or (transl_pred_rec p1, transl_pred_rec p2)
  in transl_pred_rec p
