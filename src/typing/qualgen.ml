(*open Types

module L = Lightenv

include L*)

(*let collect_under_lambda = false 

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

module M = Map.make(Types.type_expr)
module S = Set.make(String)
module Si = Set.make(int)

(* on load: walk the AST for idents.
 * read qualpats, generate ordering of quals
 * *)

(* tymap: map from shapes to all idents of that shape *)
(* idset: set of all idents *)
(* intset: set of all int constants. ignored or set {0,1} if lquals set? *)

let tymap = ref M.empty   
let idset = ref S.empty
let intset = ref Si.empty

let addm (typ, id) = 
  let id = Ident.name id in
    tymap := M.add typ (S.add (M.get typ !tymap) id) !tymap

let rec qbound_idents pat = 
    let ptyp = pat.type_desc in
    match pat.pat_desc with 
      Tpat_var id -> addm (ptyp, id) 
      | Tpat_alias(p, id) -> qbound_idents p; addm (ptyp, id)
      | Tpat_or(p, _, _) -> qbound_idents p
      | d -> Typedtree.iter_pattern_desc qbound_idents d

let is_function e =
  match e.exp_desc with
    | Texp_function(_, _) -> true
    | _ -> false 

let iter_bind in_l (p, e) =
  let is_f = is_function e in
  let ps = if in_l || is_f then [] else qbound_idents p in
  let es = if in_l then [] else iter_pats false e in
    ps @ es

let rec iter_pats in_l e =
  let ve e =
    let etyp = e.exp_type in
      match e.exp_desc with
        | Texp_function(al, _) ->
            iter_bind 
        | Texp_let(_, bl, ee) ->
            

let rec visit_binding (pat, exp) as pe = 
  let vp pat = qbound_idents pat in
  let rec visit_bind_exp e =
    let etyp = e.exp_type in
    match e.exp_desc with
    | (_, Texp_function(al, _)) ->
       (C.flap (fun (p, e) -> vp p) @ 
       (if !more_qls then C.flap (fun (p, e) -> ve e) al else [])
    | (p, Texp_let(_, bl, e)) ->

    | (p, e) ->
  in 
  (fun (p, e) -> 
    let es = visit_bind_exp e in
    let ps = if List.length es != 0 then C.expand visit_pat p in
      es @ ps) pe



*)

type base = int list  
type var_base_int = int * base

let decode (x, b) =
  let f d (x, ds) =
    ((x / d), ds @ [(x mod d)]) in
  snd (List.fold_right f x b)



    









(* Bookkeeping for qualifier generation *)


(*let constants : int list ref = ref []

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
    StringSet.elements (List.fold_left (fun s q -> StringSet.add q s) StringSet.empty multiset_quals) *)
