open Types

module L = Lightenv

include L

let collect_under_lambda = false 

(* visitor for qualgen *)

let visit_str sstr = 
  let visit_str_exp = function 
    | Tstr_eval(_) ->
        (*Some(visit_exp exp)*) None
    | Tstr_qualifier(_, _) ->
        None
    | Tstr_type(_) -> 
        None
    | Tstr_value(_, bindings) -> 
        Some(flap visit_binding bindings)
  in
  List.flatten (map_partial visit_str_exp sstr)

let 


let rec visit_binding (pat, exp) as pe = 
  let v_p p =
    let ptyp = p.pat_type in
    match p.pattern_desc with
    | Tpat_tuple(pl)
        (pl, [])
    | Tpat_any
        ([], [])
    | Tpat_var(id)
        ([], [(Ident.name id, ptyp)])
    | Tpat_alias(pat, id) 
        (pat, [(Ident.name id, ptyp)])
    | Tpat_variant(_, _, _)
    | Tpat_construct(_, _)
        ([], []) (* xx fix me *)
    | Tpat_record(lpl)
        (List.map snd lpl, []) 
    | Tpat_array(pl)
        (pl, []) 
    | Tpat_or(p1, p2, _)
        ([p1, p2], [])
  in
  let v_pp pl =
    C.expand v_p pl
  in
  let rec visit_bind_exp e =
    let etyp = e.exp_type in
    match e.exp_desc with
    | Texp_function(al, _) ->
       (C.flap (fun (a, b) -> C.expand visit_pat a) @ 
       (C.flap (fun (a, b) -> visit_bind_exp b) al)
    | _ -> []
  in 
  (fun (p, e) -> 
    let es = visit_bind_exp e in
    let ps = if List.length es != 0 then C.expand visit_pat p in
      es @ ps) pe

















(* Bookkeeping for qualifier generation *)


let constants : int list ref = ref []

let add_constant i =  
   try List.find (fun n -> n = i) !constants
      with Not_found -> constants := i::(!constants); -1

let labels : Types.type_expr list t ref = ref empty 

let add_label (lbl, t) =
  if mem lbl !labels then
    let ot = find lbl !labels in
    labels := (add lbl (t::ot) !labels)
  else
    labels := (add lbl [t] !labels)


let rec esc s oc nc = 
  let find_sp s = try String.index s oc with Not_found -> -1 in
  let next_sp = find_sp s in
  if next_sp = -1 then s else (String.fill s next_sp 1 nc; esc s oc nc)

let single_simple_qualif x fx i =
  let rels = ["<="; ">="; "!="] in
  let prels = ["_LE_"; "_GE_"; "_NE_"] in
  let ufx = String.uppercase fx in
  let _ = esc ufx ' ' '_' in 
  let _ = esc ufx '.' '_' in
  let _ = esc ufx '-' '_' in
  let _ = esc ufx '(' '_' in
  let _ = esc ufx ')' '_' in
  let ui = String.uppercase i in
  let _ = esc ui ' ' '_' in
  let _ = esc ui '.' '_' in
  let _ = esc ui '-' '_' in
  let _ = esc ui '(' '_' in
  let _ = esc ui ')' '_' in
  let mk_qualif op pop = Printf.sprintf "qualif Q%s%s%s(%s) : %s %s %s" ufx pop ui x fx op i in
  List.map2 mk_qualif rels prels
  
module StringSet = Set.Make(String)

let single_int_qualif path =
  let name = Path.name path in
  List.append (single_simple_qualif "_AA" "_AA" name)
   (List.append (single_simple_qualif "_AA" "Array.length _AA" name)
   (List.append (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" name)
      (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" name)))


let single_const_qualif i =
  let i = if i >= 0 then string_of_int i 
                    else Printf.sprintf "(0 - %d)" (abs i) in
  List.concat [(single_simple_qualif "_AA" "_AA" i);
              (single_simple_qualif "_AA" "Array.length _AA" i);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" i);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" i)]

let single_array_qualif path =
  let name = Path.name path in
  let array_length = "Array.length " ^ name in
  List.append (single_simple_qualif "_AA" "_AA" array_length)
              (single_simple_qualif "_AA" "Array.length _AA" array_length)

let single_bigarray_array2_qualif path =
  let name = Path.name path in
  let bigarray_array2_dim1 = "Bigarray.Array2.dim1 " ^ name in
  let bigarray_array2_dim2 = "Bigarray.Array2.dim2 " ^ name in
  List.concat [(single_simple_qualif "_AA" "_AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "_AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim1 _AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Bigarray.Array2.dim2 _AA" bigarray_array2_dim2);
              (single_simple_qualif "_AA" "Array.length _AA" bigarray_array2_dim1);
              (single_simple_qualif "_AA" "Array.length _AA" bigarray_array2_dim2);]
               
let dump_qualifs () =
  let consts = 0::1::!constants  in
  let lbls = !labels in
  let const_qualifs = List.concat (List.map single_const_qualif consts) in
  let lfilter p b ty = 
      let ty = Ctype.repr ty in
      b || 
      match ty with
        {desc = Tconstr(p', _, _)} ->
          Path.same p p'
        | t -> false
  in 
  let filter p path ts = if List.fold_left (lfilter p) false ts then Some path
                                                                else None
  in
  let int_lbls = mapfilter (filter Predef.path_int) lbls in
  let int_qualifs = List.concat (List.map single_int_qualif int_lbls) in
  let arr_lbls = mapfilter (filter Predef.path_array) lbls in
  let arr_qualifs = List.concat (List.map single_array_qualif arr_lbls) in
  let bigarray_lbls = mapfilter (filter (Builtins.ext_find_type_path "array2")) lbls in
  let bigarray_qualifs = List.concat (List.map single_bigarray_array2_qualif bigarray_lbls) in
  let multiset_quals = List.concat [bigarray_qualifs; arr_qualifs; int_qualifs; const_qualifs] in
    StringSet.elements (List.fold_left (fun s q -> StringSet.add q s) StringSet.empty multiset_quals) 
