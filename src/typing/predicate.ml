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

(* patterns *)

type patpexpr =
    PPInt of int list
  | PVar of Path.t list
  | PFunApp of Longident.t * patpexpr list 
  | PBinop of patpexpr * binop list * patpexpr

type tpat =
  | PTrue
  | PAtom of patpexpr * binrel list * patpexpr
  | PIff of patpexpr * tpat
  | PNot of tpat
  | PAnd of tpat * tpat
  | POr of tpat * tpat

(************)

type pexpr =
    PInt of int 
  | Var of Path.t
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr
  | Field of string * pexpr

type t =
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of pexpr * t
  | Not of t
  | And of t * t 
  | Or of t * t

let pprint_rel = function
    Eq -> "="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let rec pprint_pexpr ppf = function
  | PInt n ->
      fprintf ppf "%d" n
  | Var x ->
      fprintf ppf "%s" (Path.unique_name x)
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" f pprint_pexpr pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
      in fprintf ppf "@[(%s@ %a@ %a)@]" opstr pprint_pexpr p pprint_pexpr q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr pexp f

let rec pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%s@ %a@ %a)@]" (pprint_rel rel) pprint_pexpr p pprint_pexpr q
  | Iff (px, q) ->
      fprintf ppf "@[(<=> %a@;<1 2>%a)@]" pprint_pexpr px pprint q
  | Not p ->
      fprintf ppf "@[(not@ %a)@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[(and@;<1 2>%a@;<1 2>%a)@]" flatten_conjuncts p flatten_conjuncts q
  | Or (p, q) ->
      fprintf ppf "@[(or@;<1 2>%a@;<1 2>%a)@]" flatten_disjuncts p flatten_disjuncts q
and flatten_conjuncts ppf = function
  | And (And (p1, p2), And (q1, q2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@;<1 0>%a@]"
        flatten_conjuncts p1 flatten_conjuncts p2
        flatten_conjuncts q1 flatten_conjuncts q2
  | And (And (p1, p2), q)
  | And (q, And (p1, p2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@]"
        pprint q flatten_conjuncts p1 flatten_conjuncts p2
  | p -> pprint ppf p
and flatten_disjuncts ppf = function
  | Or (Or (p1, p2), Or (q1, q2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@;<1 0>%a@]"
        flatten_disjuncts p1 flatten_disjuncts p2
        flatten_disjuncts q1 flatten_disjuncts q2
  | Or (Or (p1, p2), q)
  | Or (q, Or (p1, p2)) ->
      fprintf ppf "@[%a@;<1 0>%a@;<1 0>%a@]"
        pprint q flatten_disjuncts p1 flatten_disjuncts p2
  | p -> pprint ppf p

let equals(p, q) = Atom(p, Eq, q)

let (==.) p q = equals (p, q)

let (!=.) p q = Atom (p, Ne, q)

let (>=.) p q = Atom (p, Ge, q)

let (>.) p q = Atom (p, Gt, q)

let (<=.) p q = Atom (p, Le, q)

let (<.) p q = Atom (p, Lt, q)

let (&&.) p q = And (p, q)

let (||.) p q = Or (p, q)

let (!.) p = Not p

let (<=>.) p q = Iff (p, q)

let implies(p, q) = (!. p) ||. q

let (=>.) p q = implies (p, q)

let expand_iff = function
  | Iff (px, q) -> ((px ==. PInt 1) &&. q) ||. ((px ==. PInt 0) &&. (!. q))
  | _ -> assert false

let big_and = function
  | c :: cs -> List.fold_left (&&.) c cs
  | [] -> True

let big_or = function
  | c :: cs -> List.fold_left (||.) c cs
  | [] -> Not True

let rec pexp_map_vars f pexp =
  let rec map_rec = function
      Var x -> f x
    | FunApp (fn, e) ->
        FunApp (fn, map_rec e)
    | Binop (e1, op, e2) ->
        Binop (map_rec e1, op, map_rec e2)
    | Field (f, pexp) ->
        Field (f, map_rec pexp)
    | e ->
        e
  in map_rec pexp

let rec map_vars f pred =
  let rec map_rec = function
      True ->
        True
    | Atom (e1, rel, e2) ->
        Atom (pexp_map_vars f e1, rel, pexp_map_vars f e2)
    | Iff (px, q) -> Iff (pexp_map_vars f px, map_rec q)
    | Not p ->
        Not (map_rec p)
    | And (p, q) ->
        And (map_rec p, map_rec q)
    | Or (p, q) ->
        Or (map_rec p, map_rec q)
  in map_rec pred

let subst v x pred = map_vars (fun y -> if Path.same x y then v else Var y) pred

let apply_substs subs pred =
  let substitute p (x, e) = subst e x p in List.fold_left substitute pred subs

let rec instantiate_named_vars subs pred =
  map_vars (fun y -> Var (List.assoc (Path.name y) subs)) pred

let vars p =
  let rec exp_vars_rec vars = function
      PInt _ -> vars
    | Var x -> x::vars
    | FunApp(_, e) ->
        exp_vars_rec vars e
    | Binop(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
    | Field(f, e) ->
        exp_vars_rec vars e
  in
  let rec vars_rec vars = function
      True ->
        vars
    | Atom(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
    | Iff (px, q) ->
      let vars' = exp_vars_rec vars px in
        vars_rec vars' q
    | Not p ->
        vars_rec vars p
    | And(p1, p2)
    | Or(p1, p2) ->
        let vars' = vars_rec vars p1 in
          vars_rec vars' p2
  in
    vars_rec [] p

let tuple_nth pexp n =
  FunApp ("__tuple_nth_" ^ (string_of_int n), pexp)

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
      | Ppredexp_app (f, e) ->
	  FunApp (f, transl_pexpression e)
      | Ppredexp_binop (e1, op, e2) ->
	  Binop (transl_pexpression e1, transl_op op, transl_pexpression e2)
      | Ppredexp_field (f, r) ->
          Field (f, Var (Path.mk_ident r))
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
      | Ppred_true -> True
      | Ppred_atom (e1, rel, e2) ->
	  Atom (transl_pexpression e1, transl_rel rel, transl_pexpression e2)
      | Ppred_not p -> Not (transl_pred_rec p)
      | Ppred_and (p1, p2) -> And (transl_pred_rec p1, transl_pred_rec p2)
      | Ppred_or (p1, p2) -> Or (transl_pred_rec p1, transl_pred_rec p2)
  in transl_pred_rec p
