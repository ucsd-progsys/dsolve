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

open Format
open Predicate

module C = Common

type t = Path.t * Path.t * Predicate.t

let compare = compare

let pprint ppf (_, _, pred) = Predicate.pprint ppf pred

let apply x (_, y, p) = Predicate.subst x y p

(* The user specifies qualifiers as open predicates - i.e., the variables named
   in the qualifier may not yet be in scope at the time of definition.  But
   we want qualifiers that refer to OCaml paths, which are unique, not
   variable names, which can appear multiple times.  Using variable names
   instead of unique paths would cause trouble with the following expression:

   let a = 1 in
   let x = a + 1 in   (* x has type {v : int | v > a} *)
   let a = 3 in ...   (* x's type is now incorrect *)

   This function replaces all instances of named variables in a qualifier with
   the unique paths of the same name in the given environment.  It returns
   None if a variable in the qualifier is not found in the environment. *)
let instantiate varmap (path, valu, pred) =
  (* Don't instantiate the bound variable *)
  let varmap = (Path.ident_name_crash valu, valu) :: varmap in
    try Some (path, valu, Predicate.instantiate_named_vars varmap pred)
    with Not_found -> None

let vars (path, valu, pred) =
  C.maybe_list (List.map (fun x -> if Path.same x valu then None else Some (Path.ident_name_crash x)) (Predicate.vars pred))

let fix_bound_vars vm ps =
  let paths = fst (List.split ps) in
  let names = List.map Path.name paths in
    List.fold_left2 (fun vm n p -> C.StringMap.add n [p] vm) vm names paths

(* in qualifier.ml to avoid an odd dependency problem that breaks the build *)
let expand_about vm p =
  let rec e_rec vm = function
      PInt x -> [PInt x]
    | Var p -> 
        let p = Path.ident_name_crash p in
        List.rev_map (fun x -> Var x) (C.StringMap.find p vm)
    | FunApp (s, ps) ->
        let ess = List.map (e_rec vm) ps in
          List.rev_map (fun x -> FunApp (s, x)) (C.lflap ess)
    | Binop (e1, b, e2) ->
        C.tflap2 (e_rec vm e1, e_rec vm e2) (fun a c -> Binop (a, b, c))
    | Field (f, e1) ->
        List.rev_map (fun e -> Field(f, e)) (e_rec vm e1)
    | Ite (t, e1, e2) ->
        C.tflap3 (t_rec vm t, e_rec vm e1, e_rec vm e2) (fun a b c -> Ite (a, b, c))

  and t_rec vm = function
      True -> [True]
    | Atom(e1, b, e2) -> 
        C.tflap2 (e_rec vm e1, e_rec vm e2) (fun a c -> Atom (a, b, c))
    | Iff(e, t) ->
        C.tflap2 (t_rec vm e, t_rec vm t) (fun a b -> Iff (a, b))
    | Not t ->
        List.rev_map (fun a -> Not a) (t_rec vm t)
    | And (t1, t2) ->
        C.tflap2 (t_rec vm t1, t_rec vm t2) (fun a b -> And (a, b))
    | Or (t1, t2) ->
        C.tflap2 (t_rec vm t1, t_rec vm t2) (fun a b -> Or (a, b))
    | Implies (t1, t2) ->
        C.tflap2 (t_rec vm t1, t_rec vm t2) (fun a b -> Implies (a, b))
    | Forall (p, q) ->
        List.rev_map (fun a -> Forall (p, a)) (t_rec (fix_bound_vars vm p) q)
    | Exists (p, q) ->
        List.rev_map (fun a -> Exists (p, a)) (t_rec (fix_bound_vars vm p) q)
    | Boolexp e ->
        List.rev_map (fun a -> Boolexp a) (e_rec vm e) in
  t_rec vm p

let instantiate_about vm (path, valu, pred) = 
  let vm = C.StringMap.add (Path.ident_name_crash valu) [valu] vm in
    try List.rev_map (fun x -> (path, valu, x)) (expand_about vm pred)
    with Not_found -> []

let map_pred f (p, v, pred) = 
  let (v', pred') = f (v, pred) in
    (p, v', pred')

let may_not_be_tautology (p, v, pred) =
  not (Predicate.is_taut pred)
