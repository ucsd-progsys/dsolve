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

let abstract_app_shape paths out_shape in_shapes =
  let f i o = 
    match o with
      Fabstract(a, _, _) ->
        Path.same i a 
      | _ -> false
  in if (List.length paths = List.length in_shapes) && 
        (List.for_all2 f paths in_shapes) 
          then out_shape else Funknown

(* ext_find_type_path isn't initialized until sometime late in ocaml startup, so
   we'll suspend this and force it when we know it's safe *)
let builtin_fun_app_shapes = lazy(
  let array2_path = Builtins.ext_find_type_path "array2" in
    [("Array.length", abstract_app_shape [Predef.path_array] uInt);
     ("Bigarray.Array2.dim1", abstract_app_shape [array2_path] uInt);
     ("Bigarray.Array2.dim2", abstract_app_shape [array2_path] uInt);
     (tag_function, (function [Fsum _] -> uInt | _ -> Funknown))]
)

let rec app_to_fun funf =
  match funf with
      Farrow(_, f1, f2) ->
        (fun ps -> 
           match ps with
             p :: ps ->
               (*let _ = Format.printf "@[%a@.%a@.%b@]@.@." pprint f1 pprint p
                 (subt f1 p) in *)
               if (subt f1 p || subt (unfold f1) p) then (app_to_fun f2) ps else Funknown
           | [] -> Funknown)
    | f -> 
        (fun ps -> 
           match ps with
             [] -> f
           | _ -> Funknown)

let get_by_name (n, env) =
  let s n' v = n = Path.name n' in
  let cs = Lightenv.filterlist s env in
    match cs with 
      c :: [] -> c
    | c :: cs -> failwith (Printf.sprintf "too many definitions of %s" n)
    | [] -> assert false (* this is going to break a number of fascinating things in bitv *)

let get_by_name =
  let tbl = Hashtbl.create 17 in
    fun n env -> Common.do_memo tbl get_by_name (n, env) n (* (n, env) is proper, but this will do when we only have measures *)

let get_app_shape s env =
  try
    List.assoc s (Lazy.force builtin_fun_app_shapes)
  with Not_found -> 
    app_to_fun (get_by_name s env)

exception IllFormed

let pred_is_well_typed env p =
  let rec get_expr_shape = function
  | P.PInt _ -> uInt
  | P.Var x -> (try Lightenv.find x env with Not_found -> raise IllFormed)
  | P.FunApp (s, p') -> 
      let y = ((get_app_shape s) env) (List.map get_expr_shape p') in
        y
  | P.Binop (p1, op, p2) ->
      if same_shape (get_expr_shape p1) uInt then
        if same_shape (get_expr_shape p2) uInt then
          uInt
        else
          raise IllFormed
      else
        raise IllFormed
  | P.Field (name, r) ->
      begin match get_expr_shape r with
        | Fsum (_, _, [(_, fs)], _) ->
            (* pmr: maybe we need to switch to ids for this *)
            let is_referenced_field (name2, _, _) = String.compare (Ident.name name) (Ident.name name2) = 0 in
              if List.exists is_referenced_field fs then
                (match (List.find is_referenced_field fs) with (_, f, _) -> f)
              else raise IllFormed
        | f -> raise IllFormed
      end
  | P.Ite (t, e1, e2) ->
      if pred_shape_is_bool t then
        let e1_shp = get_expr_shape e1 in
          if same_shape e1_shp (get_expr_shape e2) then e1_shp else raise IllFormed
      else
        raise IllFormed
  and pred_shape_is_bool = function
  | P.True -> true
  | P.Not p -> pred_shape_is_bool p 
  | P.Or (p1, p2)  
  | P.And (p1, p2) ->
      if pred_shape_is_bool p1 then
        pred_shape_is_bool p2
      else
        raise IllFormed
  | P.Atom (p1, rel, p2) -> 
      let p1_shp = get_expr_shape p1 in
      let p2_shp = get_expr_shape p2 in
        ((same_shape p1_shp p2_shp) (*&& not(same_shape p1_shp Funknown)*))
        || ((same_shape p1_shp uBool) && (same_shape p2_shp uInt))
        || ((same_shape p1_shp uInt) && (same_shape p2_shp uBool))
  | P.Iff (px, q) ->
      if same_shape (get_expr_shape px) uInt then
        pred_shape_is_bool q
      else
        raise IllFormed
  in pred_shape_is_bool p

let refinement_well_formed env solution r qual_expr =
  try
    Bstats.time "pred_well_typed" (pred_is_well_typed env) (refinement_predicate solution qual_expr r)
  with IllFormed ->
    false
