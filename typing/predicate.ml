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

type pexpr =
    PInt of int 
  | Var of Path.t
  | FunApp of Path.t * pexpr list  
  | Binop of pexpr * binop * pexpr
  | Field of Path.t * pexpr
  | Ite of t * pexpr * pexpr

and t =
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of t * t
  | Not of t
  | And of t * t 
  | Or of t * t
  | Forall of (Path.t * Parsetree.prover_t) list * t
  | Exists of (Path.t * Parsetree.prover_t) list * t
  | Implies of t * t
  | Boolexp of pexpr

type t_or_pexpr =
    Pexpr of pexpr
  | Pt of t

let pprint_rel = function
    Eq -> "="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let strip_meas s =
  let start = try 1 + (String.rindex s '.') with Not_found -> 0 in
  try 
    if String.sub s start 6 = "_meas_" then 
      let pre  = String.sub s 0 start in
      let post = String.sub s (start + 6) ((String.length s) - start - 6) in
      pre ^ post
    else s
  with Invalid_argument _ -> s

let rec pprint_pexpr ppf = function
  | PInt n ->
      if n < 0 then fprintf ppf "(0 - %d)" (-n)
      else fprintf ppf "%d" n
  | Var x ->
      fprintf ppf "%s" (C.path_name x)
  | FunApp (f, pexp) ->
      fprintf ppf "@[(%s %a)@]" (strip_meas (C.path_name f)) (C.pprint_many false "" pprint_pexpr) pexp
  | Binop (p, op, q) ->
      let opstr = match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Div -> "/"
      in fprintf ppf "@[(%a %s %a)@]" pprint_pexpr p opstr pprint_pexpr q
  | Field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pexpr pexp (C.path_name f)
  | Ite (t, e1, e2) ->
      fprintf ppf "@[if (%a) then (%a) else (%a)@]" pprint t pprint_pexpr e1 pprint_pexpr e2

and pprint ppf = function
  | True ->
      fprintf ppf "true"
  | Atom (p, rel, q) ->
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pexpr p (pprint_rel rel) pprint_pexpr q
  | Iff (px, q) ->
      fprintf ppf "@[(%a@ iff@;<1 2>%a)@]" pprint px pprint q
  | Not p ->
      fprintf ppf "@[(not %a)@]" pprint p
  | And (p, q) ->
      fprintf ppf "@[(%a@ &&@;<1 2>@;<1 2>%a)@]" pprint p pprint q
  | Or (p, q) ->
      fprintf ppf "@[(%a@ ||@;<1 2>@;<1 2>%a)@]" pprint p pprint q
  | Implies (p, q) ->
      fprintf ppf "@[(%a =>@;<1 2>%a)@]" pprint p pprint q
  | Forall (p, q) ->
      let p = List.map (fun (n, t) -> (C.path_name n) ^ ": " ^ (C.prover_t_to_s t)) p in
      fprintf ppf "@[(forall@ (%a.@ %a))@]" (C.pprint_many false "," C.pprint_str) p pprint q
  | Exists (p, q) ->
      let p = List.map (fun (n, t) -> (C.path_name n) ^ ": " ^ (C.prover_t_to_s t)) p in
      fprintf ppf "@[(exists@ (%a.@ %a))@]" (C.pprint_many false "," (C.pprint_str)) p pprint q
  | Boolexp e ->
      fprintf ppf "@[(? (%a))@]" pprint_pexpr e

let pprint_t_or_pexpr ppf = function
  | Pexpr e -> pprint_pexpr ppf e
  | Pt t -> pprint ppf t

let rec pprint_pattern ppf p =
  match p.ppredpat_desc with
  | Ppredpat_true ->
      fprintf ppf "true"
  | Ppredpat_atom (p, rel, q) ->
      let relstring = function
      | Pred_eq -> "="
      | Pred_ne -> "!="
      | Pred_gt -> ">"
      | Pred_lt -> "<"
      | Pred_le -> "<="
      | Pred_ge -> ">=" in
      let relstring = function
      | [x] -> relstring x
      | xs -> "[ " ^ (String.concat ", " (List.map relstring xs)) ^ " ]" in
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pattern_pexpr p (relstring rel) pprint_pattern_pexpr q
  | Ppredpat_iff (px, q) ->
      fprintf ppf "@[(%a@ iff@;<1 2>%a)@]" pprint_pattern px pprint_pattern q
  | Ppredpat_not p ->
      fprintf ppf "@[(not@ %a)@]" pprint_pattern p
  | Ppredpat_and (p, q) ->
      fprintf ppf "@[(%a@ &&@;<1 2>@;<1 2>%a)@]" pprint_pattern p pprint_pattern q
  | Ppredpat_or (p, q) ->
      fprintf ppf "@[(%a@ ||@;<1 2>@;<1 2>%a)@]" pprint_pattern p pprint_pattern q
  | Ppredpat_implies (p, q) ->
      fprintf ppf "@[(%a ->@;<1 2>%a)@]" pprint_pattern p pprint_pattern q
  | Ppredpat_forall (p, q) ->
      let p = List.map (fun (n, t) -> n ^ ": " ^ (C.prover_t_to_s t)) p in
      fprintf ppf "@[(forall@ (%a.@ %a))@]" (C.pprint_list ", " C.pprint_str) p pprint_pattern q
  | Ppredpat_exists (p, q) ->
      let p = List.map (fun (n, t) -> n ^ ": " ^ (C.prover_t_to_s t)) p in
      fprintf ppf "@[(exists@ (%a.@ %a))@]" (C.pprint_list ", " (C.pprint_str)) p pprint_pattern q
  | Ppredpat_boolexp e ->
      fprintf ppf "@[(? (%a))@]" pprint_pattern_pexpr e

and pprint_pattern_pexpr ppf p =
  match p.ppredpatexp_desc with
  | Ppredpatexp_any_int ->
      fprintf ppf "^"
  | Ppredpatexp_int n ->
      let f n = if n < 0 then fprintf ppf "(0 - %d) " (-n)
                else fprintf ppf "%d " n in
      (match n with
      | [n] -> f n
      | ns -> fprintf ppf "@[["; ignore (List.map f ns); fprintf ppf "]@]")
  | Ppredpatexp_var x ->
      let f n = fprintf ppf "%s" (C.l_to_s n) in
      (match x with
      | [x] -> f x
      | xs -> fprintf ppf "@[["; ignore (List.map f xs); fprintf ppf "]@]")
  | Ppredpatexp_mvar x ->
      fprintf ppf "~%s" x
  | Ppredpatexp_funapp (f, pexp) ->
      fprintf ppf "@[(%s@ %a)@]" (C.l_to_s f) (C.pprint_list " " pprint_pattern_pexpr) pexp
  | Ppredpatexp_binop (p, op, q) ->
      let opstr op = match op with
        | Predexp_plus -> "+"
        | Predexp_minus -> "-"
        | Predexp_times -> "*"
				| Predexp_div -> "/" in
      let opstr = function
        | [o] -> opstr o
        | os -> "{ " ^ (String.concat ", " (List.map opstr os)) ^ " }" in
      fprintf ppf "@[(%a@ %s@ %a)@]" pprint_pattern_pexpr p (opstr op) pprint_pattern_pexpr q
  | Ppredpatexp_field (f, pexp) ->
      fprintf ppf "@[%a.%s@]" pprint_pattern_pexpr pexp f
  | Ppredpatexp_ite (t, e1, e2) ->
      fprintf ppf "@[if@ (%a)@ then@ (%a)@ else@ (%a)@]" pprint_pattern t pprint_pattern_pexpr e1 pprint_pattern_pexpr e2

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

let (?.) e = Boolexp e

let tag_function = Path.mk_ident "__tag"
let tag x = FunApp(tag_function, [x])

let skolem_function = Path.mk_ident "__skolem"
let skolem =
  let sk = ref 0 in fun () ->
    incr sk; FunApp(skolem_function, [PInt !sk])

let find_const c =
  C.int_of_tag (Env.lookup_constructor (Longident.Lident c) Env.initial).cstr_tag

let big_and = function
  | c :: cs -> List.fold_left (&&.) c cs
  | [] -> True

let big_or = function
  | c :: cs -> List.fold_left (||.) c cs
  | [] -> Not True

let rec pexp_map_subexps f = function
  | FunApp (fn, es)    -> FunApp (fn, List.map f es)
  | Binop (e1, op, e2) -> Binop (f e1, op, f e2)
  | Field (fld, e)     -> Field (fld, f e)
  | Ite (e1, e2, e3)   -> Ite (e1, f e2, f e3)
  | e                  -> e

and pexp_map_subpreds f = function
  | Ite (e1, e2, e3) -> Ite (f e1, e2, e3)
  | p                -> p

and pexp_map f e =
  f (pexp_map_subpreds (map f) (pexp_map_subexps (pexp_map f) e))

and map_subpreds f = function
  | Iff (p, q)     -> Iff (f p, f q)
  | Not (p)        -> Not (f p)
  | And (p, q)     -> And (f p, f q)
  | Or (p, q)      -> Or (f p, f q)
  | Implies (p, q) -> Implies (f p, f q)
  | Forall (p, q)  -> Forall (p, f q)
  | Exists (p, q)  -> Exists (p, f q)
  | p              -> p

and map_subexps f = function
  | Atom (e1, rel, e2) -> Atom (f e1, rel, f e2)
  | Boolexp (e)        -> Boolexp (f e)
  | p                  -> p

and map f p =
  map_subexps (pexp_map f) (map_subpreds (map f) p)

let iter f p =
  ignore (map (fun x -> f x; x) p)

let pexp_iter f e =
  ignore (pexp_map (fun x -> f x; x) e)

let map_var f = function
  | Var x -> f x
  | e     -> e

let pexp_map_vars f e =
  pexp_map (map_var f) e

let bound_vars = function
  | Forall (vs, _) | Exists (vs, _) -> fst (List.split vs)
  | _                               -> []

let rec map_vars f p =
  let bound = bound_vars p in
  let f     = function x when not (List.mem x bound) -> f x | y -> Var y in
    map_subexps (pexp_map (map_var f)) (map_subpreds (map_vars f) p)

let pred_or_pexp_map_vars f = function
  | Pexpr e -> Pexpr (pexp_map_vars f e)
  | Pt t -> Pt (map_vars f t)

let map_fun f = function
  | FunApp (fn, e) -> FunApp (f fn, e)
  | e              -> e

let pexp_map_funs f e =
  pexp_map (map_fun f) e

let map_funs f pred =
  map (map_fun f) pred

let pred_or_pexp_map_funs f = function
  | Pexpr e -> Pexpr (pexp_map_funs f e)
  | Pt t -> Pt (map_funs f t)

let subst_var v x y =
  if Path.same x y then v else Var y

let chain_substs fsub subs f =
  List.fold_right (fun (x, e) p -> fsub e x p) subs f

let pexp_subst v x exp =
  pexp_map_vars (subst_var v x) exp

let pexp_apply_substs subs exp =
  chain_substs pexp_subst subs exp

let subst v x pred =
  map_vars (subst_var v x) pred

let apply_substs subs pred =
  chain_substs subst subs pred

let instantiate_named_vars subs pred =
  map_vars (fun y -> try Var (List.assoc (Path.ident_name_fail y) subs)
                     with Not_found -> Var y
                        | Failure _ -> (try Var (List.assoc (Path.name y) subs)
                            with Not_found -> Var y)) pred

let unexp f = function
  | True -> ([], [])
  | Atom (e1, _, e2) -> ([], f e1 @ f e2)
  | Iff (p, q) -> ([p; q], [])
  | Not p -> ([p], [])
  | And (p, q) | Or (p, q) | Implies (p, q) -> ([p; q], [])
  | Forall (p, q) | Exists (p, q) -> ([q], [])
  | Boolexp e -> ([], f e)

let rec exp_vars_unexp = function
  | PInt _ -> ([], [])
  | Var x -> ([], [x])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (_, es) -> (es, [])
  | Field (_, e) -> ([e], [])
  | Ite (t, e1, e2) -> ([e1; e2], vars t)

and exp_vars e =
  let vs = ref [] in
    ignore (pexp_map_vars (fun v -> vs := v :: !vs; Var v) e);
    !vs

and exp_funs_unexp = function
  | PInt _ -> ([], [])
  | Var _ -> ([], [])
  | Binop (e1, _, e2) -> ([e1; e2], [])
  | FunApp (s, es) -> (es, [s])
  | Field (_, e) -> ([e], [])
  | Ite (t, e1, e2) -> ([e1; e2], funs t)

and exp_funs e =
  Misc.expand exp_funs_unexp [e] []

and var_unexp p = unexp exp_vars p
and funs_unexp p = unexp exp_funs p

and vars e =
  let vs = ref [] in
    ignore (map_vars (fun v -> vs := v :: !vs; Var v) e);
    !vs

and funs e =
  Misc.expand funs_unexp [e] []

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
 
let conjuncts_unexp = function
  | And (p, q) -> ([p; q], [])
  | True -> ([], [])
  | p -> ([], [p])

let conjuncts p = 
  Misc.expand conjuncts_unexp [p] []

let is_taut = function
  | Atom(e1, Eq, e2) -> e1 = e2
  | True -> true
  | _ -> false

let contains_div_by_zero p =
  let b = ref false in
  iter (function Binop (_, Div, PInt 0) -> b := true | _ -> ()) p; !b

let is_contra = 
  let falses = [Not True; Atom (PInt 0, Eq, PInt 1); Atom (PInt 1, Eq, PInt 0)] in
  fun p -> List.mem p falses 
