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

let find_or_fail var env = try Lightenv.find var env with Not_found -> assert false

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

let get_by_name n env =
  let s n' v = n = Path.name n' in
  let cs = Lightenv.filterlist s env in
    match cs with 
      c :: [] -> c
    | c :: cs -> failwith (Printf.sprintf "too many definitions of %s" n)
    | [] -> assert false (* this is going to break a number of fascinating things in bitv *)

let get_app_shape s env =
  try
    List.assoc s (Lazy.force builtin_fun_app_shapes)
  with Not_found -> 
    app_to_fun (get_by_name s env) 

let pred_is_well_typed env p =
  let rec get_expr_shape = function
  | Predicate.PInt _ -> uInt
  | Predicate.Var x -> find_or_fail x env
  | Predicate.FunApp (s, p') -> 
      let y = (get_app_shape s env) (List.map get_expr_shape p') in
        y
  | Predicate.Binop (p1, op, p2) ->
      let p1_shp = get_expr_shape p1 in
      let p1_int = same_shape p1_shp uInt in
      let p2_shp = get_expr_shape p2 in
      let p2_int = same_shape p2_shp uInt in
      if p1_int && p2_int then uInt else Funknown
  | Predicate.Field (name, r) ->
      begin match get_expr_shape r with
        | Fsum (_, _, [(_, fs)], _) ->
            (* pmr: maybe we need to switch to ids for this *)
            let is_referenced_field (name2, _, _) = String.compare (Ident.name name) (Ident.name name2) = 0 in
              if List.exists is_referenced_field fs then
                (match (List.find is_referenced_field fs) with (_, f, _) -> f)
              else Funknown
        | f -> Funknown
      end
  and pred_shape_is_bool = function
  | Predicate.True -> true
  | Predicate.Not p -> pred_shape_is_bool p 
  | Predicate.Or (p1, p2)  
  | Predicate.And (p1, p2) -> (pred_shape_is_bool p1) && (pred_shape_is_bool p2)
  | Predicate.Atom (p1, rel, p2) -> 
      let p1_shp = get_expr_shape p1 in
      let p2_shp = get_expr_shape p2 in
        ((same_shape p1_shp p2_shp) (*&& not(same_shape p1_shp Funknown)*))
        || ((same_shape p1_shp uBool) && (same_shape p2_shp uInt))
        || ((same_shape p1_shp uInt) && (same_shape p2_shp uBool))
  | Predicate.Iff (px, q) -> same_shape (get_expr_shape px) uInt && pred_shape_is_bool q
  in pred_shape_is_bool p

let refinement_well_formed env solution r qual_expr =
  let pred = refinement_predicate solution qual_expr r in
  let var_bound v = Lightenv.mem v env in
  let bound_by_name s = (Lightenv.filterlist (fun n v -> s = Path.name n) env) != [] in
  let well_scoped = List.for_all var_bound (Predicate.vars pred) && List.for_all bound_by_name (Predicate.funs pred) in
    well_scoped && pred_is_well_typed env pred
