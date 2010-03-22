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

open Parsetree (* must be opened before typedtree *)
open Types
open Typedtree
open Btype
open Format
open Asttypes

module C = Common
module PM = C.PathMap
module T = Typetexp
module Pat = Pattern
module M = Miscutil
module P = Predicate
module Le = Lightenv

(**************************************************************)
(************** Type definitions: Refinements *****************)
(**************************************************************)

type substitution = Path.t * P.pexpr
type dep_sub      = string * string

type qvar       = Path.t
type refexpr    = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type 'a prerecref = 'a list list
type recref       = refinement prerecref

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

(**************************************************************)
(***************** Type definitions: Frames *******************)
(**************************************************************)

type ('a, 'b) preframe =
  | Fvar      of Path.t * int * dep_sub list * 'a
  | Frec      of Path.t * 'b prerecref * 'a
  | Fsum      of Path.t * (Path.t * 'b prerecref) option * ('a, 'b) preconstr list * 'a
  | Fabstract of Path.t * ('a, 'b) preparam list * Ident.t * 'a
  | Farrow    of pattern_desc * ('a, 'b) preframe * ('a, 'b) preframe

and ('a, 'b) preparam  = Ident.t * ('a, 'b) preframe * variance
and ('a, 'b) preconstr = constructor_tag * (string * ('a, 'b) preparam list)
and variance           = Covariant | Contravariant | Invariant

type param  = (refinement, refinement) preparam
type constr = (refinement, refinement) preconstr

type t = (refinement, refinement) preframe

let path_tuple = Path.mk_ident "tuple"

exception LabelLikeFailure of t * t

(**************************************************************)
(**************** Type environments ***************************)
(**************************************************************)

(* this is a bit shady: we memoize by string only to save the compare*)
exception Found_by_name of t

let find_by_name env s =
   try (Le.iter (fun p f -> if Path.name p = s then raise (Found_by_name f)) env; raise Not_found)
    with Found_by_name t -> t 

let prune_env_funs env = Le.fold (fun p f xs -> (function Farrow _ -> xs | _ -> p :: xs) f) env []

let env_ignore_list = ["Pervasives"; "Open_"; "FP_"; "false"; "true"; "Array"; "String"; "Big"; "None"; "Some"; "Random"; "[]"; "()"]

let prune_background env = Le.filter
  (fun p _ -> 
    let p = Path.name p in
    List.for_all (fun pre -> not(C.has_prefix pre p)) env_ignore_list && not(C.tmpstring p)) env

(**************************************************************)
(**************** Constructed type accessors ******************)
(**************************************************************)

let params_frames ps =
  List.map (fun (_, f, _) -> f) ps

let params_ids ps =
  List.map (fun (i, _, _) -> i) ps

let constr_params (_, (_, p)) =
  p

let constr_param_frames c =
  params_frames (constr_params c)

let constr_app_params f (t, (n, ps)) =
  (t, (n, f ps))

let constr_app_params2 f (t, (n, ps1)) (_, (_, ps2)) =
  (t, (n, f ps1 ps2))

let constrs_tag_params t cs =
  snd (List.assoc t cs)

let record_field f n = match f with
  | Fsum (_, _, [c], _) -> List.nth (constr_param_frames c) n
  | _                   -> failwith "record_field called on non-record"

(**************************************************************)
(************************* Iterators **************************) 
(**************************************************************)

let apply_params_frames f ps =
  List.map (fun (p, fr, v) -> (p, f fr, v)) ps

let apply_constrs_params_frames f cs =
  List.map (constr_app_params (apply_params_frames f)) cs

let rec map f = function
  | (Fvar _ | Frec _) as fr  -> f fr
  | Fsum (p, ro, cs, r)      -> f (Fsum (p, ro,  apply_constrs_params_frames (map f) cs, r))
  | Fabstract (p, ps, id, r) -> f (Fabstract (p, map_params f ps, id, r))
  | Farrow (x, f1, f2)       -> f (Farrow (x, map f f1, map f f2))

and map_params f ps =
  apply_params_frames (map f) ps

let rec map_labels f fr = 
  map (map_label_map f) fr

and map_label_map f = function
    Farrow (x, a, b) -> Farrow (f x, a, b)
  | Fabstract (p, ps, id, r) ->
      let id = if C.empty_list ps then id else
        (List.hd (Typedtree.pat_desc_bound_idents (f (Tpat_var id)))) in
        Fabstract(p, map_param_labels f ps, id, r)
  | fr -> fr

and map_param_labels f ps =
  let i2p i = Tpat_var i in
    List.map (fun (i, fr, v) -> (List.hd (Typedtree.pat_desc_bound_idents (f (i2p i))), map_labels f fr, v)) ps  

let rec iter_labels f fr =
  let f' p = f p; p in
    ignore (map_labels f' fr)

let map_recref f rr =
  List.map (fun r -> List.map f r) rr

let rec map_refinements_map f = function
  | Frec (p, rr, r)          -> Frec (p, map_recref f rr, f r)
  | Fvar (p, level, s, r)    -> Fvar (p, level, s, f r)
  | Fsum (p, ro, cs, r)      -> Fsum (p, M.may_map (fun (p, rr) -> (p, map_recref f rr)) ro, cs, f r)
  | Fabstract (p, ps, id, r) -> Fabstract (p, ps, id, f r)
  | Farrow _ as f            -> f

let map_refinements f fr =
  map (map_refinements_map f) fr

let map_refexprs f fr =
  map_refinements (fun r -> List.map f r) fr

let map_qualifiers f fr =
  map_refexprs (fun (subs, (qs, vs)) -> (subs, (List.rev_map f qs, vs))) fr

let recref_fold f rr l =
  List.fold_right f (List.flatten rr) l

let recref_iter f rr =
  List.iter f (List.flatten rr)

let rec refinement_fold f l = function
  | Frec (_, rr, r) -> f r (recref_fold f rr l)
  | Fvar (_, _, s, r) -> f r l
  | Fsum (_, ro, cs, r) ->
      f r (List.fold_left (refinement_fold f) (match ro with Some (_, rr) -> recref_fold f rr l | None -> l) (C.flap constr_param_frames cs))
  | Fabstract (_, ps, _, r) ->
      f r (List.fold_left (refinement_fold f) l (params_frames ps))
  | Farrow (_, f1, f2) ->
      refinement_fold f (refinement_fold f l f1) f2

let rec refinement_iter f = function
  | Frec (_, rr, r) -> recref_iter f rr; f r
  | Fvar (_, _, s, r) -> f r
  | Fsum (_, ro, cs, r) ->
      (match ro with Some (_, rr) -> recref_iter f rr | None -> ()); 
      f r; List.iter (refinement_iter f) (C.flap constr_param_frames cs)
  | Fabstract (_, ps, _, r) ->
      List.iter (refinement_iter f) (params_frames ps); f r
  | Farrow (_, f1, f2) ->
      refinement_iter f f1; refinement_iter f f2

(******************************************************************************)
(*************************** Type level manipulation **************************)
(******************************************************************************)

let generic_level = -Btype.generic_level

let current_level = ref (-1)

let uninitialized_level = 0

let rec initialize_type_expr_levels t =
  let t = repr t in
    if t.level >= uninitialized_level then t.level <- !current_level;
    Btype.iter_type_expr initialize_type_expr_levels t

let begin_def () =
  decr current_level

let end_def () =
  incr current_level

let generalize_map = function
  | Fvar (p, level, s, r) ->
      let level = if level < !current_level then generic_level else level in
        Fvar (p, level, s, r)
  | f -> f

let generalize f =
  map generalize_map f

(**************************************************************)
(****************** Refinement manipulation *******************) 
(**************************************************************)

let mk_refinement subs qconsts qvars =
  [(subs, (qconsts, qvars))]

let empty_refinement =
  mk_refinement [] [] []

let const_refinement qs =
  mk_refinement [] qs []

let refinement_is_empty qes =
  List.for_all (function (_, ([], [])) -> true | _ -> false) qes

let recref_is_empty rr =
  List.for_all (List.for_all refinement_is_empty) rr

let false_refinement =
  mk_refinement [] [(Path.mk_ident "false", Path.mk_ident "V", P.Not (P.True))] []

let apply_refinement r = function
  | Fvar (p, level, s, _)    -> Fvar (p, level, s, r)
  | Fsum (p, rr, cs, _)      -> Fsum (p, rr, cs, r)
  | Fabstract (p, ps, id, _) -> Fabstract (p, ps, id, r)
  | f                        -> f

  (*need to apply subs when refinement is pulled somehow..*)
let get_refinement = function
  | Fvar (_, _, _, r) | Fsum (_, _, _, r) | Fabstract (_, _, _, r) -> Some r
  | _ -> None

let append_refinement res' f =
  match get_refinement f with
    | Some res -> apply_refinement (res @ res') f
    | None     -> f

let set_recref rr = function
  | Frec (p, _, r)                -> Frec (p, rr, r)
  | Fsum (p, Some (rp, _), cs, r) -> Fsum (p, Some (rp, rr), cs, r)
  | _                             -> assert false

let get_recref = function
  | Frec (_, rr, _) | Fsum (_, Some (_, rr), _, _) -> Some rr
  | _                                              -> None

let merge_recrefs rr rr' =
  List.map2 (List.map2 (@)) rr rr'

let append_recref rr' f = match get_recref f with
  | Some rr -> set_recref (merge_recrefs rr rr') f
  | None    -> f

let eager_apply subs qs =
  List.rev_map (Qualifier.map_pred (C.app_snd (P.apply_substs subs))) qs 

let refexpr_apply_subs subs' (subs, qexprs) =
  match qexprs with
  | (qconsts, [])    -> (subs, (eager_apply subs' qconsts, []))
  | (qconsts, qvars) -> (subs' @ subs, qexprs)

let apply_subs subs f =
  map_refexprs (refexpr_apply_subs subs) f

let refinement_qvars r =
  C.flap (fun (_, (_, qvars)) -> qvars) r

exception Found

let has_kvars f = 
  try ignore (map_refinements 
    (fun r -> if not (C.empty_list (refinement_qvars r)) then raise Found; r)
      f); false with Found -> true

(**************************************************************)
(*********** Conversions to/from simple refinements ***********)
(**************************************************************)

let split_refexpr (qc, qv) (subs, (qconsts, qvars)) =
  (qc @ List.map (fun q -> (subs, Qconst q)) qconsts,
   qv @ List.map (fun k -> (subs, Qvar k)) qvars)

let ref_to_simples r =
  List.fold_left split_refexpr ([], []) r

let ref_of_simple = function
  | (subs, Qconst q) -> mk_refinement subs [q] []
  | (subs, Qvar v)   -> mk_refinement subs [] [v]

(******************************************************************************)
(****************************** Constructor tags ******************************)
(******************************************************************************)

let maybe_tag_qualifier (_, v, pred) =
  match pred with
     P.Atom (P.FunApp (tag_fun, [(P.Var v')]), P.Eq, P.PInt t) when v = v' && tag_fun = P.tag_function -> Some (C.tag_of_int t)
   | _ -> None

let find_tag_single ts r =
  let (_, (qs, _)) = r in
    (C.maybe_list (List.map maybe_tag_qualifier qs)) @ ts

let find_tag rs =
  C.only_one "too many tags in constructed value" (List.fold_left find_tag_single [] rs)

(**************************************************************)
(************************* Shapes *****************************) 
(**************************************************************)

let shape f =
  map_refinements (fun _ -> empty_refinement) f

let is_shape f =
  refinement_fold (fun r b -> refinement_is_empty r && b) true f

(* known bug: these don't treat constructor names properly. *)

let same_shape t1 t2 =
  let vars = ref [] in
  let ismapped p q = try snd (List.find (fun (p', _) -> Path.same p p') !vars) = q with
      Not_found -> vars := (p, q) :: !vars; true in
  let rec sshape = function
      (Fsum(p, ro, cs, _), Fsum(p', ro', cs', _)) ->
        Path.same p p' && params_sshape (C.flap constr_params cs) (C.flap constr_params cs') &&
          (ro = ro' || match (ro, ro') with (Some (rp, _), Some (rp', _)) -> ismapped rp rp' | _ -> false)
    | (Fabstract(p, ps, _, _), Fabstract(p', ps', _, _)) ->
        Path.same p p' && params_sshape ps ps'
    | (Fvar (p, _, _, _), Fvar (p', _, _, _)) | (Frec (p, _, _), Frec (p', _, _)) ->
        ismapped p p'
    | (Farrow(_, i, o), Farrow(_, i', o')) ->
        sshape (i, i') && sshape (o, o')
    | t -> false
  and params_sshape ps qs =
    C.same_length ps qs && List.for_all sshape (List.combine (params_frames ps) (params_frames qs))
  in sshape (t1, t2)
       
let fid () = Path.mk_ident "p"
let maybe_assoc p l =
  try
    Some (List.assoc p l) 
  with Not_found -> None

let rec subt t1 t2 eq inst =
  (* yes, i inlined a union find :( *)
  let map = ref inst in
  let pmap = ref eq in
  let get_ind p =
    try
      List.assoc p !pmap
    with Not_found -> 
      let p' = fid () in 
      let _ = pmap := (p, p') :: !pmap in
        p' in
  let get_set p =
    let p' = get_ind p in
    List.filter (fun (x, y) -> y = p') !pmap in
  let ismapped p f =
    let p' = get_ind p in
    try
      same_shape (List.assoc p' !map) f
    with Not_found -> (map := (p', f) :: !map; true) in
  let join p' q' =
    pmap := ((List.map (fun (x, _) -> (x, p')) (get_set q')) @ !pmap) in
  let equiv p q =
    let p' = get_ind p in
    let q' = get_ind q in
    if p' = q' then true else
      match (maybe_assoc p' !map, maybe_assoc q' !map) with
          (Some f1, Some f2) -> 
          if same_shape f1 f2 then
            let _ = join p' q' in true
            else false
        | (None, Some f) | (Some f, None) ->
          let _ = map := ((p', f) :: !map) in
          let _ = join p' q' in true
        | (None, None) ->
          let _ = join p' q' in true in
  let rec s_rec (f1, f2) = 
    match (f1, f2) with
    | (Fsum(p, ro, cs, _), Fsum(p', ro', cs', _)) ->
        Path.same p p' && p_s (C.flap constr_params cs) (C.flap constr_params cs') &&
       (ro = ro' || match (ro, ro') with (Some (_, _), Some(_, _)) -> true | _ -> false) 
    | (Fabstract(p, ps, _, _), Fabstract(p', ps', _, _)) ->
        Path.same p p' && p_s ps ps'
    | (Frec(_, _, _), Frec(_, _, _)) ->
        true
    | (Frec(_, _, _), _) ->
        false
        (* s_rec (f2, t1) *)
    | (_, Frec(_, _, _)) ->
        (*assert*) false (* assume that the LHS is folded up *)
    | (Fvar(p1, _, _, _), Fvar(p2, _, _, _)) ->
        equiv p1 p2
    | (Fvar(p, _, _, _), _) -> 
        ismapped p f2
    | (Farrow(_, i, o), Farrow(_, i', o')) ->
        s_rec(i, i') && s_rec(o, o')
    | t -> false 
  and p_s ps qs =
    List.for_all s_rec (List.combine (params_frames ps) (params_frames qs)) in
  (s_rec (t1, t2), !pmap, !map) (* relies on deterministic o of eval *)

let subti t1 t2 =
  subt t1 t2 [] []

let subtis t1 t2 =
  (fun (x, _, _) -> x) (subti t1 t2)

let map_inst eq inst f =
  let get_ind p =
    try List.assoc p eq with Not_found -> p in
  let mapped p =
    let p = get_ind p in
    try
      Some (List.assoc p inst)
    with Not_found -> None in
  let m_inst = function
    | Fvar (p, _, _, _) as ofr -> (match mapped p with Some fr -> fr | None -> ofr)
    | fr -> fr in 
  map m_inst f

(**************************************************************)
(******************** Frame pretty printers *******************)
(**************************************************************)

let top_sym = "⊤"

let bot_sym = "⊥"

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[(%s@ ->@ %a)@]" (Path.unique_name path) P.pprint_pexpr pexp

let pred_of_qual subs q =
  let p = Qualifier.apply (P.Var (Path.mk_ident "V")) q in
  P.apply_substs subs p

let flatten_refinement res = 
  List.fold_left 
    (fun (ps, svs) (s, (qs, vs)) -> 
      (ps @ (List.map (pred_of_qual s) qs)), 
      (svs @ (List.map (fun v -> (s,v)) vs)))
    ([],[]) res

let pprint_psub ppf (s, v) =
  if !Clflags.print_subs then
    fprintf ppf "[%a] %s" (C.pprint_many false " " pprint_sub) s (C.path_name v) 
  else
    fprintf ppf "%s" (C.path_name v) 

let pprint_refinement ppf res = 
  match flatten_refinement res with
  | [], []  -> 
      fprintf ppf "%s" top_sym
  | ps, _ when List.exists P.is_contra ps ->  
      fprintf ppf "%s" bot_sym
  | ps, svs -> 
      fprintf ppf "@[<hv 0>%a%a@]" 
        (C.pprint_many true "" P.pprint) ps 
        (C.pprint_many false ""  pprint_psub) svs 

let rec pprint_pattern ppf = function
  | Tpat_any -> 
      fprintf ppf "_"
  | Tpat_var x -> 
      fprintf ppf "%s" (C.ident_name x)
  | Tpat_tuple pats -> 
      fprintf ppf "(%a)" pprint_pattern_list pats 
  | Tpat_construct (cstrdesc, pats) ->
      begin match (repr cstrdesc.cstr_res).desc with
        | Tconstr (p, _, _) -> 
            fprintf ppf "%s(%a)" (Path.name p) pprint_pattern_list pats
        | _ -> assert false end
  | _ -> assert false

and pprint_pattern_list ppf pats =
  let ds = List.map (fun p -> p.pat_desc) pats in
  C.pprint_many false "," pprint_pattern ppf ds 

let wrap_refined r ppf pp =
  if List.for_all (function (_, ([], [])) -> true | _ -> false) r then
    (fprintf ppf "@["; pp ppf; fprintf ppf "@]")
  else
    (fprintf ppf "@[{"; pp ppf; fprintf ppf " |@ %a}@]" pprint_refinement r)

let pprint_refs ppf rs =
  fprintf ppf "‹@[%a›@]" (C.pprint_many false "," pprint_refinement) rs

let pprint_recref ppf rr =
  let lp, rp  = "«", "»" in 
  if not (recref_is_empty rr) 
  then fprintf ppf "%s@[%a%s@]" lp (C.pprint_many false "," pprint_refs) rr rp

let level_suffix l = 
  if l = generic_level then "P" else "M"

let rec pprint ppf = function
  | Frec (path, rr, r) ->
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "@[%a %s@]" pprint_recref rr (C.path_name path))
  | Fvar (a, level, s, r) ->
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "'%s%s%a" (C.path_name a) (level_suffix level) 
                  (C.pprint_many false "" (fun ppf (s, s') -> fprintf ppf "[%s/%s]" s s')) s)
  
  | Fsum (path, None, cs, r) -> (* vanilla sum *)
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "%s. @[<hv 0>%a@]" (C.path_name path) print_sum cs)
   
  | Fsum (path, _ , _, r) when !Clflags.hide_rectypes ->
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf " %s " (C.path_name path))

  | Fsum (path, Some (rp, rr), cs, r) -> (* rec sum: not printing rr *)
      let la, ra  = "«", "»" in 
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "%s%s%s %s. @[<hv 0>%a@]" la (C.path_name path) ra (C.path_name rp) print_sum cs)
  | Farrow (pat, f, f') -> 
      fprintf ppf "@[<hv 0>(%a:%a) ->@ %a@]" pprint_pattern pat pprint f pprint f'
  | Fabstract (path, [], id, r) ->
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "@[%s@]" (C.path_name path)) 
  | Fabstract (path, params, id, r) ->
      wrap_refined r ppf 
      (fun ppf -> fprintf ppf "@[%s: %a %s@]"
                  (C.path_name (C.i2p id)) print_prd params (C.path_name path)) 

and print_sum ppf = function
  | [] -> ()
  | cs -> fprintf ppf "@[<hv 0>%a@]" (C.pprint_many true "+" pprint_constructor) cs

and print_prd ppf = function
  | []  -> ()
  | prd -> fprintf ppf "(@[<hv 0>%a)@]" (C.pprint_many true "," print_bind) prd

and pprint_constructor ppf (_, (n, ps)) = 
   fprintf ppf "%s%a" n print_prd ps

and print_bind ppf (name, f, _) = 
   fprintf ppf "%s:%a" (C.ident_name name) pprint f
 
let rec pprint_fenv ppf fenv =
  Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (C.path_name k) pprint v) fenv

(******************************************************************************)
(********************************** Unfolding *********************************)
(******************************************************************************)

let rec apply_refs rs ps = match (rs, ps) with
  | (r :: rs, (i, f, v) :: ps) ->
      let (ip, i') = (Path.Pident i, Ident.create (Ident.name i)) in
      let sub      = (ip, P.Var (Path.Pident i')) in
      let ps       = List.map (fun (i, f, v) -> (i, apply_subs [sub] f, v)) ps in
      let rs       = List.map (List.map (refexpr_apply_subs [sub])) rs in
        (i', append_refinement r f, v) :: apply_refs rs ps
  | ([], []) -> []
  | _        -> assert false

let apply_recref_constrs rr cs =
  List.map2 (fun rs -> constr_app_params (apply_refs rs)) rr cs

let apply_recref rr = function
  | Fsum (p, ro, cs, r) -> Fsum (p, ro, apply_recref_constrs rr cs, r)
  | _                   -> assert false

let replace_recvar f f' = match f with
  | Fsum (p, Some (rp, rr), cs, r) ->
      map (function Frec (rp', rr', r') when Path.same rp rp' -> append_refinement r' (append_recref rr' f') | f -> f) (Fsum (p, None, cs, r))
  | _ -> f

let unfold f =
  replace_recvar f (apply_refinement empty_refinement f)

let unfold_applying f =
  let f' = match get_recref f with Some rr -> apply_recref rr f | None -> f in
    replace_recvar f' (apply_refinement empty_refinement f)

(**************************************************************)
(********* Polymorphic and qualifier instantiation ************) 
(**************************************************************)

type bind = Bpat of pattern_desc | Bid of Ident.t

let dep_sub_to_sub binds scbinds env (s, s') =
  let i2sub i = (Ident.name i, C.i2p i) in
  let bind = function
    | Bpat p -> List.map i2sub (Typedtree.pat_desc_bound_idents p)
    | Bid i -> [i2sub i] in
  let binds = C.flap bind binds in
  let scbinds = C.flap bind scbinds in
  let c i =
    try List.assoc i binds
      with Not_found -> Le.find_path i env in
  try (c s', P.Var (List.assoc s scbinds))
    with Not_found -> failwith (sprintf "Could not bind dependent substitution %s to paths" s)

let apply_dep_subs subs = function
    Fvar (p, i, _, r) -> Fvar (p, i, subs, r)
  | _                 -> assert false

let instantiate_dep_subs vars subs =
  let c i =
    try Path.name (List.assoc i vars) with Not_found -> i in
  List.map (C.app_pr c) subs
  
(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate env fr ftemplate =
  let binds = ref [] in
  let vars  = ref [] in
  let vmap p ft =
    try List.assoc p !vars with Not_found -> vars := (p, ft) :: !vars; ft in
  let rec inst scbinds f ft =
    match (f, ft) with
      | (Fvar (p, level, s, r), _) when level = generic_level ->
          let instf = vmap p ft in 
          let subs  = List.map (dep_sub_to_sub !binds scbinds env) s in
          apply_subs subs (append_refinement r instf)
      | (Fvar _, _) | (Frec _, _) ->
          f
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          let nf1 = inst scbinds f1 f2 in
          let _   = binds := (Bpat l) :: !binds in
          let nf2 = inst ((Bpat l) :: scbinds) f1' f2' in
          Farrow (l, nf1, nf2)
      | (Fsum (p, ro, cs, r), Fsum(p', _, cs', _)) ->
          Fsum (p, ro, List.map2 (constr_app_params2 (inst_params scbinds)) cs cs', r)
      | (Fabstract(p, ps, id, r), Fabstract(_, ps', _, _)) ->
          let _ = binds := (Bid id) :: !binds in
          Fabstract (p, inst_params ((Bid id) :: scbinds) ps ps', id, r)
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    raise (Failure ("Instantiate"))
  and inst_params scbinds ps ps' =
    let bind_param (ps, scbinds) (p, f, v) (_, f', _) =
      binds := (Bid p) :: !binds;
      ((ps @ [(p, inst scbinds f f', v)]), (Bid p) :: scbinds) in
    fst (List.fold_left2 bind_param ([], scbinds) ps ps')
  in inst [] fr ftemplate

let instantiate_refexpr_qualifiers vars (subs, (qconsts, qvars)) =
  (subs, (List.map (fun q -> match Qualifier.instantiate vars q with Some q -> q | None -> q) qconsts, qvars))

let instantiate_ref_qualifiers vars r =
  List.map (instantiate_refexpr_qualifiers vars) r

let instantiate_recref_qualifiers vars rr =
  List.map (fun r -> List.map (instantiate_ref_qualifiers vars) r) rr

let instantiate_qualifiers vars fr =
  map_refexprs (instantiate_refexpr_qualifiers vars) fr

(**************************************************************)
(********************* Argument labeling **********************) 
(**************************************************************)

(* Label all the formals in [f] with their corresponding labels in
   [f'] and changing constant qualifiers appropriately.
   [f] and [f'] are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let label_like f f' =
  let rec label vars f f' = match (f, f') with
    | (Fvar _, Fvar (_, _, s, _)) ->
        instantiate_qualifiers vars (apply_dep_subs (instantiate_dep_subs vars s) f) 
    | (Frec _, Frec _) ->
        instantiate_qualifiers vars f
    | (Fsum (p, rro, cs1, r), Fsum (_, _, cs2, _)) ->
        let rro =
          match rro with
            | None -> None
            | Some (rv, rr) -> Some (rv, instantiate_recref_qualifiers vars rr)
        in Fsum (p, rro, label_constrs_like vars cs1 cs2, instantiate_ref_qualifiers vars r)
    | (Fabstract (p, ps, id, r), Fabstract (_, ps', id', _)) ->
        let vars' = (Ident.name id, Path.Pident id') :: vars in
          Fabstract (p, label_params_like vars' ps ps', id', instantiate_ref_qualifiers vars r)
    | (Farrow (p1, f1, f1'), Farrow (p2, f2, f2')) ->
        let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
          Farrow (p2, label vars f1 f2, label vars' f1' f2')
    | _ -> printf "@[Can't label %a like %a@.@]" pprint f pprint f'; raise (LabelLikeFailure(f, f'))
  and label_constrs_like vars cs1 cs2 =
    List.map2 (constr_app_params2 (label_params_like vars)) cs1 cs2
  and label_params_like vars ps1 ps2 = match (ps1, ps2) with
    | ([], []) -> []
    | ((i1, f1, v1) :: ps1, (i2, f2, _) :: ps2) ->
        let f1 = label vars f1 f2 in
        let vars = (Ident.name i1, Path.Pident i2) :: vars in
          (i2, f1, v1) :: label_params_like vars ps1 ps2
    | ([], x :: xs)
    | (x :: xs, []) -> raise (Failure "Label_like param mismatch")
  in label [] f f'

let record_of_params path ps r =
  Fsum(path, None, [(Cstr_constant 0, ("rec", ps))], r)

let abstract_of_params_with_labels labels p params varis id r =
  Fabstract (p, C.combine3 labels params varis, id, r)

let abstract_of_params p params varis r =
  let id = if C.empty_list params then C.dummy_id else C.abstr_elem_id () in
  Fabstract (p, C.combine3 (Miscutil.mapi (fun _ i -> C.tuple_elem_id i) params) params varis, id, r)

let tuple_of_frames fs r =
  record_of_params path_tuple (Miscutil.mapi (fun f i -> (C.tuple_elem_id i, f, Covariant)) fs) r

(******************************************************************************)
(**************************** Function application ****************************)
(******************************************************************************)

let apply_once f e = match f with
  | Farrow (p, f, f') -> apply_subs (Pattern.bind_pexpr p e) f'
  | _                 -> invalid_arg "apply called with non-function argument"

let apply f es =
  List.fold_left apply_once f es

(**************************************************************)
(******************* Fresh frames *****************************)
(**************************************************************)

let translate_variance = function
  | (true, true, true)   -> Invariant
  | (true, false, false) -> Covariant
  | (false, true, false) -> Contravariant
  | (a, b, c)            -> printf "@[Got gibberish variance (%b, %b, %b)@]@." a b c; assert false

let mutable_variance = function
  | Mutable -> Invariant
  | _       -> Covariant

let fresh_refinementvar () =
  mk_refinement [] [] [Path.mk_ident "k"]

let fresh_fvar level = Fvar (Path.mk_ident "a", level, [], empty_refinement)

let rec canonicalize t =
  begin match t.desc with
    | Tlink t' -> t.id <- t'.id; t.level <- t'.level; t.desc <- t'.desc
    | _        -> ()
  end;
  let t = repr t in
    begin match t.desc with
      | Tvar -> ()
      | _    -> t.id <- 0
    end; Btype.iter_type_expr (fun t -> ignore (canonicalize t)) t; t

let is_recursive_instance t t' =
  t.desc = t'.desc

let close_arg res arg =
  if is_recursive_instance res arg then link_type arg res

let close_constructor res args =
  List.iter (fun a -> Btype.iter_type_expr (close_arg res) a; close_arg res a) args

let instance_record field_tys param_tys =
  let _                      = Ctype.begin_def () in
  let (field_tys, param_tys) = Ctype.instance_lists field_tys param_tys in
  let _                      = Ctype.end_def () in
    (field_tys, param_tys)

let fresh_field fresh (name, muta, t) =
  (Ident.create name, fresh t, mutable_variance muta)

let fresh_record fresh env p fields t formal_tys actual_tys =
  let (names, mutas, field_tys) = C.split3 fields in
  let (field_tys, formal_tys)   = instance_record field_tys formal_tys in
  let _                         = List.iter Ctype.generalize formal_tys in
  let _                         = List.iter2 (Ctype.unify env) formal_tys actual_tys in
  let field_tys                 = List.map canonicalize field_tys in
  let _                         = close_constructor t field_tys in
  let fields                    = C.combine3 names mutas field_tys in
    record_of_params p (List.map (fresh_field fresh) fields) empty_refinement

let fresh_constructor env fresh ty name cstr =
  let _           = Ctype.begin_def () in
  let (args, res) = Ctype.instance_constructor cstr in
  let _           = Ctype.end_def () in
  let _           = List.iter Ctype.generalize args; Ctype.generalize res in
  let _           = close_constructor res args; Ctype.unify env res ty in
  let ids         = Miscutil.mapi (fun _ i -> C.tuple_elem_id i) args in
  let fs          = List.map fresh args in
  let vs          = List.map (fun _ -> Covariant) fs in
    (cstr.cstr_tag, (name, C.combine3 ids fs vs))

let fresh_sum fresh env p t names tyl =
  let (_, cds)   = List.split (Env.constructors_of_type p (Env.find_type p env)) in
    Fsum (p, None, List.map2 (fresh_constructor env fresh t) names cds, empty_refinement)

let fresh_constr fresh env p t tyl =
  let ty_decl = Env.find_type p env in
    match ty_decl.type_kind with
      | Type_abstract ->
          abstract_of_params p (List.map fresh tyl) (List.map translate_variance ty_decl.type_variance) empty_refinement
      | Type_record (fields, _, _) ->
          fresh_record fresh env p fields t ty_decl.type_params tyl
      | Type_variant (cdecls, _) ->
          let (cnames, _) = List.split cdecls in
            fresh_sum fresh env p t cnames tyl

let close_recf (rp, rr) = function
  | Fsum (p, _, cs, r) ->
      let f = Fsum (p, Some (rp, rr), cs, r) in
        begin match unfold f with
          | Fsum (_, _, cs', _) -> if cs = cs' then Fsum (p, None, cs, r) else f
          | _                   -> assert false
        end
  | f -> f

let first_binder = Char.code 'a' - 1

let next_binder = ref first_binder

let reset_binders () = next_binder := first_binder

let fresh_binder () =
  incr next_binder; Tpat_var (Ident.create (Char.escaped (Char.chr !next_binder)))

let fresh_rec fresh env (rv, rr) level t = match t.desc with
  | Tvar                 -> fresh_fvar level
  | Tarrow(_, t1, t2, _) -> Farrow (fresh_binder (), fresh t1, fresh t2)
  | Ttuple ts            -> tuple_of_frames (List.map fresh ts) empty_refinement
  | Tconstr(p, tyl, _)   -> close_recf (rv, rr) (fresh_constr fresh env p t (List.map canonicalize tyl))
  | _                    -> failwith "@[Error: Freshing unsupported type]@."

let null_refinement =
  mk_refinement [] [] []

let param_refinements freshref res args =
  List.map (fun t -> if is_recursive_instance res t || !Clflags.no_recrefs then null_refinement else freshref) args

let cstr_refinements freshref cstr =
  let (args, res) = Ctype.instance_constructor cstr in
  let _           = close_constructor res args in
  let (args, res) = (List.map canonicalize args, canonicalize res) in
    param_refinements freshref res args

let mk_recref freshref env t =
  match t.desc with
    | Tconstr (p, _, _) ->
        let ty_decl = Env.find_type p env in
          begin match ty_decl.type_kind with
            | Type_record (fields, _, _) ->
                let (_, _, field_tys)       = C.split3 fields in
                let (field_tys, formal_tys) = instance_record field_tys ty_decl.type_params in
                let field_tys               = List.map canonicalize field_tys in
                let t                       = canonicalize {t with desc = Tconstr (p, formal_tys, ref Mnil)} in
                  [param_refinements freshref t field_tys]
            | _ ->
                let (_, cstrs) = List.split (Env.constructors_of_type p ty_decl) in
                  List.map (cstr_refinements freshref) cstrs
          end
    | _ -> []

let mk_recvar env t =
  (Path.mk_ident "t", mk_recref empty_refinement env t)

let place_freshref freshf r =
  if r == null_refinement then empty_refinement else freshf ()

let rec abs_type_levels t =
  t.level <- abs t.level; Btype.iter_type_expr abs_type_levels t

let rec flip_frame_levels f =
  map (function Fvar (p, level, s, r) -> Fvar (p, -level, s, r) | f -> f) f

let kill_top_recref env t f = match f with
  | Fsum (_, Some _, _, _) -> set_recref (mk_recref null_refinement env t) f
  | _                      -> f

(* UGLY HACK ON UGLY HACK *)
let fresh_with_var_fun_get_instantiation env freshf t =
  let tbl = Hashtbl.create 17 in
  let t   = C.copy_type t in
  (* Negative type levels wreak havoc with the unify, etc. functions used in fresh_constr *)
  let _   = abs_type_levels t in
  let _   = reset_binders () in
  let rec fm t =
    let level = (repr t).level in
    let t     = canonicalize t in
    (* By freshing the parameters first, we avoid the scenario where a non-recursive type
       erroneously becomes recursive through its instantiated parameters.
       (e.g., a non-recursive 'a t instantiated with b, where b contains b t.) *)
    let _     = match t.desc with Tconstr (_, param_tys, _) -> ignore (List.map fm param_tys) | _ -> () in
      if Hashtbl.mem tbl t then
        Hashtbl.find tbl t
      else
        let (rp, rr) = mk_recvar env t in
          Hashtbl.replace tbl t (Frec (rp, rr, if !Clflags.no_recvarrefs then null_refinement else empty_refinement));
          let res = kill_top_recref env t (fresh_rec fm env (rp, rr) level t) in
            Hashtbl.replace tbl t res; res
  in (flip_frame_levels (map_refinements (place_freshref freshf) (fm t)), tbl)

let fresh_with_var_fun env freshf t =
  fst (fresh_with_var_fun_get_instantiation env freshf t)

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations. *)
let fresh env ty =
  fresh_with_var_fun env fresh_refinementvar ty

let fresh_false env ty =
  fresh_with_var_fun env (fun _ -> false_refinement) ty

let fresh_with_labels env ty f =
  label_like (fresh env ty) f

let fresh_without_vars env ty =
  fresh_with_var_fun env (fun _ -> empty_refinement) ty

let frame_var = function
  | Fvar (p, _, _, _) -> p
  | _                 -> failwith "Frame is not var"

(* UGLY HACK *)
let fresh_constructed_params_no_vars env path paramfs =
  match List.split (Env.constructors_of_type path (Env.find_type path env)) with
    | (_, cstr :: _) ->
        let _, res = Ctype.instance_constructor cstr in
          begin match res.desc with
            | Tconstr (_, paramts, _) ->
                let _      = assert (List.length paramts = List.length paramfs) in
                let f, tbl = fresh_with_var_fun_get_instantiation env (fun _ -> empty_refinement) res in
                let subst  = List.map2 (fun pt pf -> (frame_var (Hashtbl.find tbl pt), pf)) paramts paramfs in
                  map (function Fvar _ as fv -> List.assoc (frame_var fv) subst | fm -> fm) f
            | _ -> assert false
          end
    | _ -> failwith "Freshed type has no constructors!"

let rec build_uninterpreted name params = function
  | Farrow (_, f, f') ->
      let lab = Ident.create "x" in
        Farrow (Tpat_var lab, f, build_uninterpreted name (lab :: params) f')
  | f ->
      let args = List.rev_map (fun p -> P.Var (Path.Pident p)) params in
      let v    = Path.mk_ident "v" in
      let pexp = match args with [] -> P.Var name | args -> P.FunApp (name, args) in
      let r    = const_refinement [(name, v, P.Atom (P.Var v, P.Eq, pexp))] in
        apply_refinement r f

let fresh_uninterpreted env ty name =
  build_uninterpreted name [] (fresh_without_vars env ty)

(******************************************************************************)
(**************************** Constructor embedding ***************************)
(******************************************************************************)

let mk_unint_constructor f n ps =
  let frames = params_frames ps in
    build_uninterpreted n [] (List.fold_right (fun f f' -> Farrow (Tpat_var (Ident.create ""), f, f')) frames f)

let uninterpreted_constructors env ty =
  let f = fresh_without_vars env ty in
    match unfold f with
      | Fsum (_, _, cs, _) -> List.map (fun (_, (n, ps)) -> (n, mk_unint_constructor f (Path.mk_persistent n) ps)) cs
      | _                  -> invalid_arg "uninterpreted_constructors called with non-sum type"

(**************************************************************)
(********************* Pattern binding ************************) 
(**************************************************************)

let rec bind pat frame =
  let _bind = function
    | (Tpat_any, _) ->
        ([], [])
    | (Tpat_var x, f) ->
        ([], [(Path.Pident x, f)])
    | (Tpat_alias (p, x), f) ->
        ([(p.pat_desc, f)], [(Path.Pident x, f)])
    | (Tpat_tuple pats, Fsum (_, _, [(_, (_, ps))], _)) ->
        ([], bind_params (Pattern.pattern_descs pats) ps)
    | (Tpat_construct (cstrdesc, pats), f) ->
        begin match unfold_applying f with
          | Fsum (p, _, cfvs, _) ->
              ([], bind_params (Pattern.pattern_descs pats) (constrs_tag_params cstrdesc.cstr_tag cfvs))
          | _ -> assert false
        end
    | _ -> assert false
  in C.expand _bind [(pat, frame)] []

and bind_param (subs, binds) (i, f, _) pat =
  let f = apply_subs subs f in
  let subs =
    match pat with
      | Tpat_var x -> (Path.Pident i, P.Var (Path.Pident x)) :: subs
      | _          -> subs
  in (subs, bind pat f @ binds)

and bind_params pats params  =
  snd (List.fold_left2 bind_param ([], []) params pats)

let env_bind env pat frame =
  Lightenv.addn (bind pat frame) env

(**************************************************************)
(******************** Logical embedding ***********************) 
(**************************************************************)

let refexpr_apply_solution s (subs, (qconsts, qvars)) =
  (subs, (qconsts @ C.flap s qvars, []))

let apply_solution s f =
  map_refexprs (refexpr_apply_solution s) f

let refexpr_conjuncts s qual_expr ((subs, qexprs) as r) =
  let (_, (quals, _)) = refexpr_apply_solution s r in
    List.rev_map (C.compose (P.apply_substs subs) (Qualifier.apply qual_expr)) quals

let refinement_conjuncts s qexpr res =
  C.flap (refexpr_conjuncts s qexpr) res

let refinement_predicate s qvar refn =
  P.big_and (refinement_conjuncts s qvar refn)

let rec conjunct_fold cs s qual_expr f = match get_refinement f with
  | Some r -> refinement_conjuncts s qual_expr r @ cs
  | None   -> cs

let rec conjuncts s qexpr fr =
  conjunct_fold [] s qexpr fr

let predicate s qexpr fr =
  P.big_and (conjuncts s qexpr fr)
