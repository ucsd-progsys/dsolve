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

open Builtins
open Frame

module P = Predicate
module TP = TheoremProverZ3.Prover
module Le = Lightenv

exception IllFormed

let abstract_app_shape paths out_shape in_shapes =
  let f i o = 
    match o with
      Fabstract(a, _, _, _) ->
        Path.same i a 
      | _ -> false
  in if (List.length paths = List.length in_shapes) && 
        (List.for_all2 f paths in_shapes) 
          then out_shape else raise IllFormed

let builtin_fun_app_shapes =
  [((Path.name P.tag_function), (function [Fsum _] -> uInt | _ -> raise IllFormed))]

let check_and_inst f f1 f2 eq' inst' =
  let (sub, eq, inst) = subt f1 f2 eq' inst' in
    if sub then (eq, inst)
    else let (sub, eq, inst) = subt (unfold f1) f2 eq' inst' in
      if sub then (eq, inst) else raise IllFormed

let rec app_to_fun eq inst funf =
  match funf with
      Farrow(_, f1, f2) ->
        (fun ps -> 
           match ps with
             p :: ps ->
               (*let _ = Format.printf "@[%a@.%a@.END@]@.@." pprint (shape f1) pprint (shape p) in*)
               let (eq, inst) = check_and_inst subt f1 p eq inst in 
               (*let _ = Format.printf "@[OKOK@.@]" in*)
               (app_to_fun eq inst f2) ps
           | [] -> raise IllFormed)
    | f -> 
        (fun ps -> 
           match ps with
             [] -> map_inst eq inst f
           | _ -> raise IllFormed)

let get_app_shape f env =
  try
    List.assoc (Path.name f) builtin_fun_app_shapes
  with Not_found -> 
    app_to_fun [] [] (Le.find f env)

let bind_quantified env ps =
  try
    let bindings = List.map (fun (p, tn) -> (p, TP.frame_of tn)) ps in
      Le.addn bindings env
  with Not_found ->
    raise IllFormed

let pred_is_well_typed env p =
  let rec get_expr_shape env = function
  | P.PInt _ -> uInt
  | P.Var x -> (try Le.find x env with Not_found -> raise IllFormed)
  | P.FunApp (f, p') -> 
      get_app_shape f env (List.map (get_expr_shape env) p')
  | P.Binop (p1, op, p2) ->
      if same_shape (get_expr_shape env p1) uInt then
        if same_shape (get_expr_shape env p2) uInt then
          uInt
        else
          raise IllFormed
      else
        raise IllFormed
  | P.Field (name, r) ->
      begin match get_expr_shape env r with
        | Fsum (_, _, [(_, (_, fs))], _) ->
            (* pmr: maybe we need to switch to ids for this *)
            let is_referenced_field (name2, _, _) = Path.same name (Path.Pident name2) in
              if List.exists is_referenced_field fs then
                (match (List.find is_referenced_field fs) with (_, f, _) -> f)
              else raise IllFormed
        | f -> raise IllFormed
      end
  | P.Ite (t, e1, e2) ->
      if pred_shape_is_bool env t then
        let e1_shp = get_expr_shape env e1 in
          if same_shape e1_shp (get_expr_shape env e2) then e1_shp else raise IllFormed
      else
        raise IllFormed
  and pred_shape_is_bool env = function
  | P.True -> true
  | P.Not p -> if pred_shape_is_bool env p then true else raise IllFormed
  | P.Iff (p1, p2)
  | P.Or (p1, p2)  
  | P.And (p1, p2)
  | P.Implies (p1, p2) ->
      if pred_shape_is_bool env p1 then
        if pred_shape_is_bool env p2 then true else raise IllFormed
      else
        raise IllFormed
  | P.Atom (p1, rel, p2) -> 
      let p1_shp = get_expr_shape env p1 in
      let p2_shp = get_expr_shape env p2 in
        if same_shape p1_shp p2_shp then true else raise IllFormed
  | P.Exists (ps, q)
  | P.Forall (ps, q) ->
      if pred_shape_is_bool (bind_quantified env ps) q then true else raise IllFormed
  | P.Boolexp e ->
      expr_shape_is_bool env e
  and expr_shape_is_bool env e = same_shape (get_expr_shape env e) uBool in
    if pred_shape_is_bool env p then true else raise IllFormed

let pred_well_formed env p =
  try pred_is_well_typed env p with IllFormed -> false | Not_found -> false

let refinement_well_formed env solution r qual_expr =
  try
    Bstats.time "pred_well_typed" (pred_is_well_typed env) (refinement_predicate solution qual_expr r)
  with IllFormed ->
    false
