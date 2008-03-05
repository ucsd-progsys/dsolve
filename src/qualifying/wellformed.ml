open Builtins
open Frame

let rec same_shape t1 t2 =
  match (t1, t2) with
  (Fconstr(p, l, _, _), Fconstr(p', l', _, _)) ->
   (Path.same p p') && (List.for_all (fun f -> f) (List.map2 same_shape l l')) 
  | (Fvar p, Fvar p') ->
   Path.same p p'
  | (Farrow(_, i, o), Farrow(_, i', o')) ->
   (same_shape i i') && (same_shape o o')
  | (Ftuple (t1s, _), Ftuple (t2s, _)) ->
   List.for_all2 same_shape t1s t2s
  | (Frecord (p1, f1s, _), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
      let shape_rec (f1, _, _) (f2, _, _) = same_shape f1 f2 in
        List.for_all2 shape_rec f1s f2s
  | (Funknown, Funknown) -> true
  | t -> false

let find_or_fail var env = try Lightenv.find var env with Not_found -> assert false

let constr_app_shape paths out_shape in_shapes = 
  let f i o = 
    match o with
      Fconstr(a, _, _, _) ->
        Path.same i a 
      | _ -> false
  in if (List.length paths = List.length in_shapes) && 
        (List.for_all2 f paths in_shapes) 
          then out_shape else Funknown

(* ext_find_type_path isn't initialized until sometime late in ocaml startup, so
   we'll suspend this and force it when we know it's safe *)
let fun_app_shapes = lazy(
  let array2_path = Builtins.ext_find_type_path "array2" in
    [("Array.length", constr_app_shape [Predef.path_array] uInt);
     ("Bigarray.Array2.dim1", constr_app_shape [array2_path] uInt);
     ("Bigarray.Array2.dim2", constr_app_shape [array2_path] uInt);
     (Builtins.tag_function, (function [Fconstr _] -> uInt | _ -> Funknown))]
)

let pred_is_well_typed env p =
  let rec get_expr_shape = function
  | Predicate.PInt _ -> uInt
  | Predicate.Var x -> find_or_fail x env
  | Predicate.FunApp (s, p') -> (List.assoc s (Lazy.force fun_app_shapes)) (List.map get_expr_shape p')
  | Predicate.Binop (p1, op, p2) ->
      let p1_shp = get_expr_shape p1 in
      let p1_int = same_shape p1_shp uInt in
      let p2_shp = get_expr_shape p2 in
      let p2_int = same_shape p2_shp uInt in
      if p1_int && p2_int then uInt else Funknown
  | Predicate.Field (name, r) ->
      begin match get_expr_shape r with
        | Frecord (_, fs, _) ->
            let is_referenced_field (_, name2, _) = String.compare name name2 = 0 in
              if List.exists is_referenced_field fs then
                (match (List.find is_referenced_field fs) with (f, _, _) -> f)
              else Funknown
        | f -> Funknown
      end
  | Predicate.Proj (n, t) ->
      begin match get_expr_shape t with
        | Ftuple (fs, _) -> (try List.nth fs n with Failure _ -> Funknown)
        | _ -> Funknown
      end
  and pred_shape_is_bool = function
  | Predicate.True -> true
  | Predicate.Not p -> pred_shape_is_bool p 
  | Predicate.Or (p1, p2)  
  | Predicate.And (p1, p2) -> (pred_shape_is_bool p1) && (pred_shape_is_bool p2)
  | Predicate.Atom (p1, rel, p2) -> 
      let p1_shp = get_expr_shape p1 in
      let p2_shp = get_expr_shape p2 in
        begin match rel with
        | Predicate.Ne
        | Predicate.Eq ->
         ((same_shape p1_shp p2_shp) && not(same_shape p1_shp Funknown))
         || ((same_shape p1_shp uBool) && (same_shape p2_shp uInt))
         || ((same_shape p1_shp uInt) && (same_shape p2_shp uBool))
        | Predicate.Gt
        | Predicate.Ge
        | Predicate.Lt
        | Predicate.Le ->
        (same_shape p1_shp p2_shp) && (same_shape p1_shp uInt || 
                                       (function Fvar _ -> true | _ -> false) p1_shp ||
                                       same_shape p1_shp uFloat) 
        end
  | Predicate.Iff (px, q) -> same_shape (get_expr_shape px) uInt && pred_shape_is_bool q
  in pred_shape_is_bool p

let refinement_well_formed env solution r qual_var =
  let pred = refinement_predicate solution qual_var r in
  let var_bound v = Lightenv.mem v env in
  let well_scoped = List.for_all var_bound (Predicate.vars pred) in
    well_scoped && pred_is_well_typed env pred
