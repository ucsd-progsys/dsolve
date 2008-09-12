(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

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
  | Pite of tpat * patpexpr * patpexpr

and tpat =
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
  | Field of Ident.t * pexpr
  | Ite of t * pexpr * pexpr

and t =
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of pexpr * t
  | Not of t
  | And of t * t 
  | Or of t * t
  | Forall of Path.t list * t
  | Exists of Path.t list * t

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
      fprintf ppf "%s" (Common.path_name x)
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" (C.strip_meas f) (Common.pprint_list " " pprint_pexpr) pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
				| Div -> "/"
      in fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p opstr pprint_pexpr q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr pexp (Common.ident_name f)
  | Ite (t, e1, e2) ->
      fprintf ppf "@[if@ (%a)@ then@ (%a)@ else@ (%a)@]" pprint t pprint_pexpr e1 pprint_pexpr e2

and pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p (pprint_rel rel) pprint_pexpr q
  | Iff (px, q) ->
      fprintf ppf "@[(%a@ <=>@;<1 2>%a)@]" pprint_pexpr px pprint q
  | Not p ->
      fprintf ppf "@[(not@ %a)@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[(%a@ and@;<1 2>@;<1 2>%a)@]" pprint p pprint q
  | Or (p, q) ->
      fprintf ppf "@[(%a@ or@;<1 2>@;<1 2>%a)@]" pprint p pprint q
  | Forall (p, q) ->
      let p = List.map Common.path_name p in
      fprintf ppf "@[(Forall@ (%a),@ %a)@]" (Common.pprint_list ", " (Common.pprint_str)) p pprint q
  | Exists (p, q) ->
      let p = List.map Common.path_name p in
      fprintf ppf "@[(Forall@ (%a),@ %a)@]" (Common.pprint_list ", " (Common.pprint_str)) p pprint q

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

let (+-) p q = Binop (p, Plus, q)

let ( *-) p q = Binop (p, Times, q)

let (/-) p q = Binop (p, Div, q)

let (--) p q = Binop (p, Minus, q)         

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
    | Ite (t, e1, e2) ->
        Ite (map (pexp_map_vars f) t, map_rec e1, map_rec e2)
    | e ->
        e
  in map_rec pexp

and pexp_map_funs f pexp =
  let rec map_rec = function
      Var x -> Var x
    | FunApp (fn, e) ->
        FunApp (f fn, List.map map_rec e)
    | Binop (e1, op, e2) ->
        Binop (map_rec e1, op, map_rec e2)
    | Field (f, pexp) ->
        Field (f, map_rec pexp)
    | Ite (t, e1, e2) ->
        Ite (map (pexp_map_funs f) t, map_rec e1, map_rec e2)
    | e ->
        e
  in map_rec pexp

and map f pred =
  let rec map_rec = function
      True ->
        True
    | Atom (e1, rel, e2) ->
        Atom (f e1, rel, f e2)
    | Iff (px, q) -> Iff (f px, map_rec q)
    | Not p ->
        Not (map_rec p)
    | And (p, q) ->
        And (map_rec p, map_rec q)
    | Or (p, q) ->
        Or (map_rec p, map_rec q)
    | Forall (p, q) ->
        Forall (p, map_rec q)
    | Exists (p, q) ->
        Exists (p, map_rec q)
  in map_rec pred

let rec map_vars f pred =
  map (pexp_map_vars f) pred

let rec map_funs f pred =
  map (pexp_map_funs f) pred

let subst v x pred = map_vars (fun y -> if Path.same x y then v else Var y) pred

let apply_substs subs pred =
  let substitute (x, e) p = subst e x p in List.fold_right substitute subs pred

let rec instantiate_named_vars subs pred =
  map_vars (fun y -> Var (List.assoc (Path.ident_name_crash y) subs)) pred

let unexp f = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], f e1 @ f e2)
  | Iff (e, q) -> ([q], f e)
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) -> ([p; q], [])
  | Forall (p, q) | Exists (p, q) -> ([q], [])

let rec exp_vars_unexp = function
  | PInt _ -> ([], [])
  | Var x -> ([], [x])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) -> ([e], [])
  | Ite (t, e1, e2) -> ([e1; e2], vars t)

and exp_vars e =
  C.expand exp_vars_unexp [e] []

and exp_funs_unexp = function
  | PInt _ -> ([], [])
  | Var _ -> ([], [])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (s, es) -> (es, [s])
  | Field (_, e) -> ([e], [])
  | Ite (t, e1, e2) -> ([e1; e2], funs t)

and exp_funs e =
  C.expand exp_funs_unexp [e] []

and var_unexp p = unexp exp_vars p
and funs_unexp p = unexp exp_funs p

and vars e =
  C.expand var_unexp [e] []

and funs e =
  C.expand funs_unexp [e] []

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
 

