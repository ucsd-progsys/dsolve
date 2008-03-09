open Parsetree
open Asttypes
open Types
open Format

module C = Common

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

type patpexpr =
    PPInt of int list
  | PVar of Path.t list
  | PFunApp of Longident.t * patpexpr list 
  | PBinop of patpexpr * binop list * patpexpr
  | PField of string * patpexpr 
  | PProj of int * patpexpr

type tpat =
    PTrue
  | PAtom of patpexpr * binrel list * patpexpr
  | PIff of patpexpr * tpat
  | PNot of tpat
  | PAnd of tpat * tpat
  | POr of tpat * tpat

type pexpr =
    PInt of int 
  | Var of Path.t
  | FunApp of string * pexpr list  
  | Binop of pexpr * binop * pexpr
  | Field of string * pexpr
  | Proj of int * pexpr

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
      if n < 0 then fprintf ppf "(0 - %d)" (-n)
      else fprintf ppf "%d" n
  | Var x ->
      fprintf ppf "%s" ((Common.path_name ()) x) 
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" f (Common.pprint_list " " pprint_pexpr) pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
      in fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p opstr pprint_pexpr q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr pexp f
  | Proj (n, pexp) ->
      fprintf ppf "@[%a.%d@]" pprint_pexpr pexp n

let rec pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p (pprint_rel rel) pprint_pexpr q
  | Iff (px, q) ->
      fprintf ppf "@[(%a@ <=>@;<1 2>%a)@]" pprint_pexpr px pprint q
  | Not p ->
      fprintf ppf "@[(not@ %a)@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[(%a@ and@;<1 2>@;<1 2>%a)@]" flatten_conjuncts p flatten_conjuncts q
  | Or (p, q) ->
      fprintf ppf "@[(%a@ or@;<1 2>@;<1 2>%a)@]" flatten_disjuncts p flatten_disjuncts q
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

let find_const c =
  match (Env.lookup_constructor (Longident.Lident c) Env.initial).cstr_tag with
    |  Cstr_constant n -> n
    | _ -> assert false

let (int_true, int_false) = (PInt (find_const "true"), PInt (find_const "false"))

let expand_iff = function
  | Iff (px, q) -> ((px ==. int_true) &&. q) ||. ((px ==. int_false) &&. (!. q))
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
        FunApp (fn, List.map map_rec e)
    | Binop (e1, op, e2) ->
        Binop (map_rec e1, op, map_rec e2)
    | Field (f, pexp) ->
        Field (f, map_rec pexp)
    | Proj (n, pexp) ->
        Proj (n, map_rec pexp)
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
  map_vars (fun y -> Var (List.assoc (Path.ident_name_crash y) subs)) pred

let exp_vars_unexp = function
  | PInt _ -> ([], [])
  | Var x -> ([], [x])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) | Proj (_, e) -> ([e], [])

let exp_vars e =
  C.expand exp_vars_unexp [e] []

let var_unexp = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], exp_vars e1 @ exp_vars e2)
  | Iff (e, q) -> ([q], exp_vars e)
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])

let vars e =
  C.expand var_unexp [e] []

let transl_op = function
  | Predexp_plus -> Plus
  | Predexp_minus -> Minus
  | Predexp_times -> Times
  | Predexp_div -> Div
 
let transl_rel = function
  | Pred_eq -> Eq
  | Pred_ne -> Ne
  | Pred_gt -> Gt
  | Pred_ge -> Ge
  | Pred_lt -> Lt
  | Pred_le -> Le
 
let rec transl_predicate p =
  let rec transl_pexpression pexp =
    match pexp.ppredexp_desc with
      | Ppredexp_int n ->
	  PInt n
      | Ppredexp_var y ->
	  Var (Path.mk_ident y)
      | Ppredexp_app (f, e) ->
	  FunApp (f, List.map transl_pexpression e)
      | Ppredexp_binop (e1, op, e2) ->
	  Binop (transl_pexpression e1, transl_op op, transl_pexpression e2)
      | Ppredexp_field (f, r) ->
          Field (f, Var (Path.mk_ident r))
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
