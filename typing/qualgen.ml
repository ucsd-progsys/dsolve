open Typedtree
open Types
open Asttypes

module C = Common

module CTy = 
struct
  type t = Types.type_expr
  let compare = Types.TypeOps.compare
end

module TM = Map.Make(CTy)
module TS = Set.Make(CTy)
module IS = Set.Make(String)
module CS = Set.Make(struct
                       type t = int
                       let compare = compare
                     end)

let col_lev = ref 0 (* max number of lambdas to collect under *)
let ck_clev l = l <= !col_lev

let is_function e =
  match e.exp_desc with
    | Texp_function(_, _) -> true
    | _ -> false 

let findm tymap ty = try TM.find ty tymap with Not_found -> IS.empty

let mk_intset il = 
  let init = (CS.add 0 (CS.add 1 CS.empty)) in
  List.fold_left (fun is i -> CS.add i is) init il

  (* best/worst/best again hack ever *)
let init_idset = List.fold_left (fun ids id -> IS.add id ids) IS.empty ["e0"; "e1"; "e2"; "e3"; "e4"; "e5"]
    
let mk_idset pl =
  List.fold_left (fun ids (_, id) -> IS.add (Ident.name id) ids) init_idset pl

let mk_tyset pl =
  List.fold_left (fun tys (ty, _) -> TS.add ty tys) TS.empty pl

let mk_tymap pl =
  let bind tymap (t, id) =
    let id = Ident.name id in
    TM.add t (IS.add id (findm tymap t)) tymap in 
  List.fold_left bind TM.empty pl

let bound_idents n pat = 
  let pl = ref [] in
  let rec bound_idents_rec pat =
    let ptyp = pat.pat_type in
    match pat.pat_desc with 
      Tpat_var id -> pl := (ptyp, id)::!pl 
      | Tpat_alias(p, id) -> pl := (ptyp, id)::!pl; bound_idents_rec p 
      | Tpat_or(p, _, _) -> bound_idents_rec p
      | d -> Typedtree.iter_pattern_desc bound_idents_rec d in
  if ck_clev n then (bound_idents_rec pat; !pl) else []
  
let bound_idents_always pat =
  bound_idents !col_lev pat

let lq n p = if !Clflags.less_qualifs then [] else bound_idents n p
let mb_lq n = function Some p -> lq n p | None -> []

let rec visit_binding n (pat, expr) = 
  if is_function expr then 
    visit_expr (n+1) expr
  else
    let (pl, il) = visit_expr n expr in
      (List.rev_append (mb_lq n pat) pl, il)

and visit_expr n exp =
  let (pl, il) = (ref [], ref []) in
  let addi = C.add il in
  let addp = C.addl pl in
  let vb b = 
    let (pat, e) = b in
    let (ps, is) = (visit_binding n (Some pat, e)) in
    let _ = addp ps in
     C.addl il is in 
  let bi p = addp (bound_idents n p) in
  let rec ve exp =
    match exp.exp_desc with
    | Texp_let (_, bl, e2) ->
        List.iter vb bl; ve e2  
    | Texp_constant (Const_int (i)) ->
        addi i 
    | Texp_function(pl, _) -> 
        List.iter (fun (pat, e) -> bi pat; ve e) pl
    | Texp_apply (e, el) ->
        ve e; C.opt_iter ve (List.map (fun (e, _) -> e) el)
    | Texp_match (e1, pel, _) ->
        ve e1; List.iter (fun (p, e) -> addp (lq n p); ve e) pel 
    | Texp_array (el) ->
        let _ = addi (List.length el) in
        List.iter (ve) el
    | Texp_tuple (el) 
    | Texp_construct (_, el) ->
        List.iter (ve) el
    | Texp_record (el, None) ->
        List.iter (fun (l, e) -> ve e) el
    | Texp_assert (e)
    | Texp_assume (e)
    | Texp_field (e, _) ->
        ve e
    | Texp_sequence (e1, e2)
    | Texp_setfield (e1, _, e2) ->
        ve e1; ve e2
    | Texp_ifthenelse (e1, e2, e3) ->
        ve e1; ve e2; C.resi_opt ve e3
    | Texp_constant (_)
    | Texp_assertfalse
    | Texp_ident (_, _) ->
        () 
    | Texp_variant (_, _) -> assert false
    | Texp_try(_) -> assert false
    | _ -> assert false in
  ve exp; (!pl, !il) 
  
let visit_ids_expr f exp =
  let rec ve exp =
    match exp.exp_desc with
    | Texp_let (_, pl, e2) ->
        List.iter (fun (_, e) -> ve e) pl; ve e2  
    | Texp_function(pl, _) -> 
        List.iter (fun (_, e) -> ve e) pl
    | Texp_apply (e, el) ->
        ve e; C.opt_iter ve (List.map (fun (e, _) -> e) el)
    | Texp_match (e1, pel, _) ->
        ve e1; List.iter (fun (_, e) -> ve e) pel 
    | Texp_array (el) ->
        List.iter (ve) el
    | Texp_tuple (el) 
    | Texp_construct (_, el) ->
        List.iter (ve) el
    | Texp_record (el, None) ->
        List.iter (fun (l, e) -> ve e) el
    | Texp_assert (e)
    | Texp_assume (e)
    | Texp_field (e, _) ->
        ve e
    | Texp_sequence (e1, e2)
    | Texp_setfield (e1, _, e2) ->
        ve e1; ve e2
    | Texp_ifthenelse (e1, e2, e3) ->
        ve e1; ve e2; C.resi_opt ve e3
    | Texp_constant (_)
    | Texp_assertfalse ->
        ()
    | Texp_ident (p, _) ->
        f p
    | Texp_variant (_, _) -> assert false
    | Texp_try(_) -> assert false
    | _ -> assert false in
  ve exp

let vids f _ (_, exp) = 
  visit_ids_expr f exp; ([], []) 

let vb (y, o) b =
  let (t, i) = visit_binding 0 b in
    (List.rev_append t y, List.rev_append o i)

let visit_sstr f sstr = 
  let rec visit_sstr_rec sstr =
    match sstr with
     | Tstr_value (_, bl) :: srem ->
       List.fold_left f (visit_sstr_rec srem) (List.map (fun (p, e) -> (Some p, e)) bl)
     | Tstr_eval e :: srem ->
       f (visit_sstr_rec srem) (None, e)  
     | _ :: srem ->
       visit_sstr_rec srem
     | [] -> ([], []) in
  let (pl, il) = visit_sstr_rec sstr in  
  (mk_tymap pl, mk_tyset pl, mk_idset pl, mk_intset il)  

let bound_ids sstr =
  visit_sstr vb sstr

let all_ids sstr =
  let ids = ref [] in
  let f x = ids := x :: !ids in
  ignore (visit_sstr (vids f) sstr); !ids

let all_modules sstr =
  let ismod s = (String.capitalize s = s) in
  let cut s = if String.contains s '.' then String.sub s 0 (String.index s '.') else s in
  let f p = 
    let s = (Path.name p) in
    if ismod s then Some (cut s) else None in
  C.slow_mk_unique (C.maybe_list (List.map f (all_ids sstr)))
