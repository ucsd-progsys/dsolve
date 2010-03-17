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

open Typedtree
open Types
open Ctype

module P = Predicate
module C = Common

let pattern_descs = List.map (fun p -> p.pat_desc)

let rec foldvars f b p = match p with
  | Tpat_var _                                 -> f b p
  | Tpat_tuple pats | Tpat_construct (_, pats) -> List.fold_left (foldvars f) (f b p) (pattern_descs pats)
  | Tpat_any                                   -> b
  | _                                          -> assert false

let skolemize_pattern_fold s = function
  | Tpat_var x -> (x, Ident.create "_skolemized_pattern_") :: s
  | _          -> s

let skolemize_pattern p =
  foldvars skolemize_pattern_fold [] p

let _bind_vars = function
  | (Tpat_any, _)                   -> ([], [])
  | (Tpat_var x, Tpat_var y)        -> ([], [(x, y)])
  | (Tpat_var x, Tpat_alias (_, y)) -> ([], [(x, y)])
  | (Tpat_var x, _)                 -> ([], [(x, Ident.create "_unbound_")])
  | (Tpat_tuple p1s, Tpat_tuple p2s)
  | (Tpat_construct (_, p1s), Tpat_construct (_, p2s)) ->
      (List.combine (pattern_descs p1s) (pattern_descs p2s), [])
  | (p, _) -> ([], skolemize_pattern p)

let bind_vars p1 p2 = C.expand _bind_vars [(p1, p2)] []

let substitution p1 p2 =
  let vars = bind_vars p1 p2 in
    List.map (fun (x, y) -> (Path.Pident x, Predicate.Var (Path.Pident y))) vars

let bind_pexpr pat pexp =
  let rec bind_rec subs (pat, pexp) =
    match pat with
    | Tpat_any -> subs
    | Tpat_var x -> (Path.Pident x, pexp) :: subs
    | Tpat_tuple pats ->
      let pexps = Miscutil.mapi (fun pat i -> (pat.pat_desc, P.Field(Path.Pident (C.tuple_elem_id i), pexp))) pats in
        List.fold_left bind_rec subs pexps
    | _ -> failwith "Not trying to bind pexpr to exotic pattern (see tests/null_subst.ml)\n"
  in bind_rec [] (pat, pexp)

let rec same p1 p2 =
  match (p1, p2) with
  | (Tpat_var x, Tpat_var y) when x = y -> true
  | (Tpat_any, Tpat_any) -> true
  | (Tpat_tuple pats1, Tpat_tuple pats2) ->
      List.for_all2 same (pattern_descs pats1) (pattern_descs pats2)
  | _ -> false

let get_patvar_desc = function
  | Tpat_var p | Tpat_alias (_, p) -> Some (C.i2p p)
  | _                              -> None

let get_patvar p = get_patvar_desc p.pat_desc

let cstr_res_path {cstr_res = t} = match (repr t).desc with
  | Tconstr (p, _, _) -> p
  | _                 -> assert false

let named_patterns pats =
  List.map
    (fun p -> match p.pat_desc with Tpat_alias (apat, av) -> (Some (Path.Pident av), apat) | _ -> (None, p))
    pats

let named_constructor_patterns cdesc pats = function
  | Some v -> [(v, cstr_res_path cdesc, cdesc.cstr_tag, List.map get_patvar pats)]
  | None   -> []

let constructor_patterns_aux (vo, pat) = match pat.pat_desc with
    Tpat_construct(cdesc, pl) ->
      (named_patterns pl, named_constructor_patterns cdesc pl vo)
  | Tpat_alias ({pat_desc = Tpat_construct(cdesc, pl)}, _) ->
      (named_patterns [pat], named_constructor_patterns cdesc pl vo)
  | Tpat_alias _ ->
      (named_patterns [pat], [])
  | Tpat_tuple (pl) ->
      (named_patterns pl, [])
  | _ ->
      ([], [])

let constructor_patterns expvar pat =
  C.expand constructor_patterns_aux [(Some expvar, pat)] []
