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
module M = Misc
module P = Predicate
module Le = Lightenv

(**************************************************************)
(************** Type definitions: Refinements *****************)
(**************************************************************)

type substitution = Path.t * Predicate.pexpr
type dep_sub = string * string

type qvar = Path.t
type refexpr = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type recref = refinement list list

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

(**************************************************************)
(***************** Type definitions: Frames *******************)
(**************************************************************)

type t =
  | Fvar of Path.t * int * dep_sub list * refinement
  | Frec of Path.t * recref * refinement
  | Fsum of Path.t * (Path.t * recref) option * constr list * refinement
  | Fabstract of Path.t * param list * refinement
  | Farrow of pattern_desc * t * t

and param = Ident.t * t * variance

and constr = constructor_tag * (string * param list)

and variance = Covariant | Contravariant | Invariant

let path_tuple = Path.mk_ident "tuple"

(**************************************************************)
(**************** Type environments ***************************)
(**************************************************************)

let find_by_name env s =
   List.hd (Le.filterlist (fun p _ -> Path.name p = s) env)

let find_key_by_name env s =
   try List.hd (Le.filterkeylist (fun p _ -> Path.name p = s) env)
    with Failure _ -> raise Not_found

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

(**************************************************************)
(************************* Iterators **************************) 
(**************************************************************)

let rec map f = function
  | (Fvar _ | Frec _) as fr -> f fr
  | Fsum (p, ro, cs, r) ->
      f (Fsum (p, ro, List.map (constr_app_params (map_params f)) cs, r))
  | Fabstract (p, ps, r) -> f (Fabstract (p, map_params f ps, r))
  | Farrow (x, f1, f2) -> f (Farrow (x, map f f1, map f f2))

and map_params f ps =
  List.map (fun (p, fr, v) -> (p, map f fr, v)) ps

let rec map_labels f fr = 
  let f' = function Farrow (x, a, b) -> Farrow (f x, a, b) | fr -> fr in
    map f' fr

let rec iter_labels f fr =
  let f' p = f p; p in
    ignore (map_labels f' fr)

let map_recref f rr =
  List.map (fun r -> List.map f r) rr

let rec map_refinements_map f = function
  | Frec (p, rr, r) -> Frec (p, map_recref f rr, f r)
  | Fvar (p, level, s, r) -> Fvar (p, level, s, f r)
  | Fsum (p, ro, cs, r) -> Fsum (p, M.may_map (fun (p, rr) -> (p, map_recref f rr)) ro, cs, f r)
  | Fabstract (p, ps, r) -> Fabstract (p, ps, f r)
  | f -> f

let map_refinements f fr =
  map (map_refinements_map f) fr

let map_refexprs f fr =
  map_refinements (fun r -> List.map f r) fr

let recref_fold f rr l =
  List.fold_right f (List.flatten rr) l

let rec refinement_fold f l = function
  | Frec (_, rr, r) -> f r (recref_fold f rr l)
  | Fvar (_, _, s, r) -> f r l
  | Fsum (_, ro, cs, r) ->
      f r (List.fold_left (refinement_fold f) (match ro with Some (_, rr) -> recref_fold f rr l | None -> l) (C.flap constr_param_frames cs))
  | Fabstract (_, ps, r) ->
      f r (List.fold_left (refinement_fold f) l (params_frames ps))
  | Farrow (_, f1, f2) ->
      refinement_fold f (refinement_fold f l f1) f2

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
  mk_refinement [] [(Path.mk_ident "false", Path.mk_ident "V", Predicate.Not (Predicate.True))] []

let apply_refinement r = function
  | Fvar (p, level, s, _) -> Fvar (p, level, s, r)
  | Fsum (p, rr, cs, _) -> Fsum (p, rr, cs, r)
  | Fabstract (p, ps, _) -> Fabstract (p, ps, r)
  | f -> f

  (*need to apply subs when refinement is pulled somehow..*)
let get_refinement = function
  | Fvar (_, _, _, r) | Fsum (_, _, _, r) | Fabstract (_, _, r) -> Some r
  | _ -> None

let append_refinement res' f =
  match get_refinement f with
    | Some res -> apply_refinement (res @ res') f
    | None -> f

let set_recref rr = function
  | Frec (p, _, r)                -> Frec (p, rr, r)
  | Fsum (p, Some (rp, _), cs, r) -> Fsum (p, Some (rp, rr), cs, r)
  | _                             -> assert false

let get_recref = function
  | Frec (_, rr, _) | Fsum (_, Some (_, rr), _, _) -> Some rr
  | _                                           -> None

let merge_recrefs rr rr' =
  List.map2 (List.map2 (@)) rr rr'

let append_recref rr' f = match get_recref f with
  | Some rr -> set_recref (merge_recrefs rr rr') f
  | None    -> f

let refexpr_apply_subs subs' (subs, qe) =
  (subs' @ subs, qe)

let apply_subs subs f =
  map_refexprs (refexpr_apply_subs subs) f

let refinement_qvars r =
  C.flap (fun (_, (_, qvars)) -> qvars) r

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
  | (subs, Qvar v) -> mk_refinement subs [] [v]

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
    | (Fabstract(p, ps, _), Fabstract(p', ps', _)) ->
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
    | (Fabstract(p, ps, _), Fabstract(p', ps', _)) ->
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

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[(%s@ ->@ %a)@]" (Path.unique_name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pred_of_qual q =
  Qualifier.apply (Predicate.Var (Path.mk_ident "V")) q

let space ppf =
  fprintf ppf "@;<0 1>"

let pprint_refexpr ppf (subs, (qconsts, qvars)) =
    if !Clflags.print_subs then
      fprintf ppf "[%a]@;<1 0>%a@;<1 0>%a"
      pprint_subs subs
      (Oprint.print_list (fun ppf q -> Predicate.pprint ppf (pred_of_qual q)) space) qconsts
      (Oprint.print_list (fun ppf v -> fprintf ppf "%s" (C.path_name v)) space) qvars
    else
      fprintf ppf "%a@;<1 0>%a"
      (Oprint.print_list (fun ppf q -> Predicate.pprint ppf
                          (Predicate.apply_substs subs (pred_of_qual q))) space) qconsts
      (Oprint.print_list (fun ppf v -> fprintf ppf "%s" (C.path_name v)) space) qvars

let pprint_refinement ppf res =
  Oprint.print_list pprint_refexpr space ppf res

let rec pprint_pattern ppf = function
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var x -> fprintf ppf "%s" (C.ident_name x)
  | Tpat_tuple pats ->
      fprintf ppf "(%a)" pprint_pattern_list pats
  | Tpat_construct (cstrdesc, pats) ->
      begin match (repr cstrdesc.cstr_res).desc with
        | Tconstr (p, _, _) -> fprintf ppf "%s(%a)" (Path.name p) pprint_pattern_list pats
        | _ -> assert false
      end
  | _ -> assert false

and pprint_pattern_list ppf pats =
  Oprint.print_list pprint_pattern (fun ppf -> fprintf ppf ", ") ppf (List.map (fun p -> p.pat_desc) pats)

let wrap_refined ppf pp r =
  if List.for_all (function (_, ([], [])) -> true | _ -> false) r then
    pp ppf
  else
    (fprintf ppf "@[{"; pp ppf; fprintf ppf " |@;<1 2>%a}@]" pprint_refinement r)

let comma ppf =
  fprintf ppf ",@;<1 0>"

let pprint_refs ppf rs =
  fprintf ppf "@[[%a]@]" (Oprint.print_list pprint_refinement comma) rs

let pprint_recref ppf rr =
  if not (recref_is_empty rr) then fprintf ppf "@[[%a]@]" (Oprint.print_list pprint_refs comma) rr

let pprint_recopt ppf = function
  | Some (rp, rr) -> fprintf ppf "%a@;<1 0>μ%s." pprint_recref rr (C.path_name rp)
  | None          -> ()

let pprint_dep_subs ppf s = C.pprint_list "" (fun ppf (s, s') -> fprintf ppf "[%s/%s]" s s') ppf s  

let rec pprint ppf = function
  | Frec (path, rr, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a@ %s@]" pprint_recref rr (C.path_name path)) r
  | Fvar (a, level, s, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "'%s%s%a" (C.path_name a) (if level = generic_level then "P" else "M") pprint_dep_subs s) r
  | Fsum (path, ro, [], r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a%s@]" pprint_recopt ro (C.path_name path)) r
  | Fsum (path, ro, cs, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a %s @[(%a)@]@]"
                          pprint_recopt ro
                          (C.path_name path)
                          (Oprint.print_list pprint_constructor
                             (fun ppf -> fprintf ppf "@;<1 0>|| ")) cs) r
  | Farrow (pat, f, f') ->
      fprintf ppf "@[%a:@ %a@ ->@;<1 2>%a@]" pprint_pattern pat pprint1 f pprint f'
  | Fabstract (path, params, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a@ %s@]"
                          (pprint_params ",") params
                          (C.path_name path)) r
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f
 and pprint_constructor ppf (_, (n, ps)) =
    fprintf ppf "%s (%a)" n (pprint_params ",") ps
 and pprint_param ppf (name, f, _) =
  fprintf ppf "%s:@;<1 2>%a" (C.ident_name name) pprint f
 and pprint_params sep ppf ps =
  Oprint.print_list pprint_param (fun ppf -> fprintf ppf "@;<1 2>%s@;<1 2>" sep) ppf ps

let rec pprint_fenv ppf fenv =
  Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (C.path_name k) pprint v) fenv

(******************************************************************************)
(********************************** Unfolding *********************************)
(******************************************************************************)

let rec apply_refs rs ps = match (rs, ps) with
  | (r :: rs, (i, f, v) :: ps) ->
      let (ip, i') = (Path.Pident i, Ident.create (Ident.name i)) in
      let sub      = (ip, Predicate.Var (Path.Pident i')) in
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

let unfold f = (* this may not be right *)
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
      with Not_found -> find_key_by_name env i in
    try (c s', P.Var (List.assoc s scbinds))
      with Not_found -> failwith "Could not bind dependent substitution to paths"

let apply_dep_subs subs = function
    Fvar (p, i, _, r) -> Fvar (p, i, subs, r)
  | _ -> assert false

let instantiate_dep_subs vars subs =
  let c i =
    try Path.name (List.assoc i vars) with Not_found -> i in
  List.map (C.app_pr c) subs
  
(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate env fr ftemplate =
  let binds = ref [] in
  let vars = ref [] in
  let vmap p ft =
    try List.assoc p !vars with Not_found -> vars := (p, ft) :: !vars; ft in
  let rec inst scbinds f ft =
    match (f, ft) with
      | (Fvar (p, level, s, r), _) when level = generic_level ->
          let instf = vmap p ft in 
          let subs = List.map (dep_sub_to_sub !binds scbinds env) s in
          apply_subs subs (append_refinement r instf)
      | (Fvar _, _) | (Frec _, _) ->
          f
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          let nf1 = inst scbinds f1 f2 in
          let _ = binds := (Bpat l) :: !binds in
          let nf2 = inst ((Bpat l) :: scbinds) f1' f2' in
          Farrow (l, nf1, nf2)
      | (Fsum (p, ro, cs, r), Fsum(p', _, cs', _)) ->
          Fsum (p, ro, List.map2 (constr_app_params2 (inst_params scbinds)) cs cs', r)
      | (Fabstract(p, ps, r), Fabstract(_, ps', _)) ->
          Fabstract (p, inst_params scbinds ps ps', r)
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false
  and inst_params scbinds ps ps' =
    List.map2 (fun (p, f, v) (_, f', _) -> binds := (Bid p) :: !binds; (p, inst ((Bid p) :: scbinds) f f', v)) ps ps'
  in inst [] fr ftemplate

let instantiate_refexpr_qualifiers vars (subs, (qconsts, qvars)) =
  (subs, (List.map (fun q -> match Qualifier.instantiate vars q with Some q -> q | None -> q) qconsts,
          qvars))

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
    | (Frec _, Frec _) | (Fabstract _, Fabstract _) ->
        instantiate_qualifiers vars f
    | (Fsum (p, rro, cs1, r), Fsum (_, _, cs2, _)) ->
        let rro =
          match rro with
            | None -> None
            | Some (rv, rr) -> Some (rv, instantiate_recref_qualifiers vars rr)
        in Fsum (p, rro, label_constrs_like vars cs1 cs2, instantiate_ref_qualifiers vars r)
    | (Farrow (p1, f1, f1'), Farrow (p2, f2, f2')) ->
        let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
          Farrow (p2, label vars f1 f2, label vars' f1' f2')
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  and label_constrs_like vars cs1 cs2 =
    List.map2 (constr_app_params2 (label_params_like vars)) cs1 cs2
  and label_params_like vars ps1 ps2 = match (ps1, ps2) with
    | ([], []) -> []
    | ((i1, f1, v1) :: ps1, (i2, f2, _) :: ps2) ->
        let f1 = label vars f1 f2 in
        let vars = (Ident.name i1, Path.Pident i2) :: vars in
          (i2, f1, v1) :: label_params_like vars ps1 ps2
    | _ -> assert false
  in label [] f f'

let record_of_params path ps r =
  Fsum(path, None, [(Cstr_constant 0, ("rec", ps))], r)

let abstract_of_params p params varis r =
  Fabstract (p, C.combine3 (Misc.mapi (fun _ i -> C.tuple_elem_id i) params) params varis, r)

let tuple_of_frames fs r =
  record_of_params path_tuple (Misc.mapi (fun f i -> (C.tuple_elem_id i, f, Covariant)) fs) r

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
  | (true, true, true) -> Invariant
  | (true, false, false) -> Covariant
  | (false, true, false) -> Contravariant
  | (a, b, c) -> printf "@[Got gibberish variance (%b, %b, %b)@]@." a b c; assert false

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
  let ids         = Misc.mapi (fun _ i -> C.tuple_elem_id i) args in
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

let rec copy_type = function
  | {desc = Tlink t} -> copy_type t (* Ensures copied types gets target's id/level, not link's *)
  | t                -> {t with desc = Btype.copy_type_desc copy_type t.desc}

let kill_top_recref env t f = match f with
  | Fsum (_, Some _, _, _) -> set_recref (mk_recref null_refinement env t) f
  | _                      -> f

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun env freshf t =
  let tbl = Hashtbl.create 17 in
  let t   = copy_type t in
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
  in flip_frame_levels (map_refinements (place_freshref freshf) (fm t))

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

let rec build_uninterpreted name params = function
  | Farrow (_, f, f') ->
      let lab = Ident.create "x" in
        Farrow (Tpat_var lab, f, build_uninterpreted name (lab :: params) f')
  | f ->
      let args = List.rev_map (fun p -> P.Var (Path.Pident p)) params in
      let v    = Path.mk_ident "v" in
      let pexp = match args with [] -> P.Var (Path.mk_persistent name) | args -> P.FunApp (name, args) in
      let r    = const_refinement [(Path.mk_ident name, v, P.Atom (P.Var v, P.Eq, pexp))] in
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
      | Fsum (_, _, cs, _) -> List.map (fun (_, (n, ps)) -> (n, mk_unint_constructor f n ps)) cs
      | _                  -> invalid_arg "uninterpreted_constructors called with non-sum type"

(**************************************************************)
(********************* mlq translation ************************) 
(**************************************************************)

let transl_pref plist env p = 
  let fp s = 
    let b = try List.find (fun (nm, _) -> nm = s) plist with
      Not_found -> failwith (Printf.sprintf "%s not found in mlq\n" s) in
    (fun (_, p) -> p) b in
  let (v, p) =
    match p with
    | RLiteral (v, p) -> (v, p)
    | RVar s -> fp s in
  let valu = Path.mk_ident v  in
  [([], ([(C.dummy (), valu, Qualdecl.transl_patpred_single false valu env p)], []))]

let rec translate_pframe dopt env plist pf =
  let vars = ref [] in
  let getvar a = try List.find (fun b -> Path.name b = a) !vars
                   with Not_found -> let a = Path.mk_ident a in
                   let _ = vars := a::!vars in
                     a in
  let transl_lident l = match dopt with Some d -> Longident.parse (C.append_pref d (C.l_to_s l)) | None -> l in 
  let lookup l = 
    try Env.lookup_type (transl_lident l) env 
      with Not_found -> try Env.lookup_type l env
        with Not_found -> raise (T.Error(Location.none, T.Unbound_type_constructor l)) in
  let transl_pref = transl_pref plist env in
  let rec transl_pframe_rec pf =
    match pf with
    | PFvar (a, subs, r) -> Fvar (getvar a, generic_level, subs, transl_pref r)
    | PFrec (a, rr, r) -> Frec (getvar a, transl_recref rr, transl_pref r)
    | PFsum (l, rro, cs, r) -> transl_sum l rro cs r
    | PFconstr (l, fs, r) -> transl_constr l fs r
    | PFarrow (v, a, b) ->
        let pat = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem (Path.Pident id) !vars then failwith "Redefined variable";
                vars := Path.Pident id :: !vars; Tpat_var id
          | None -> fresh_binder ()
        in Farrow (pat, transl_pframe_rec a, transl_pframe_rec b)
    | PFtuple (fs, r) -> tuple_of_frames (List.map transl_pframe_rec fs) (transl_pref r)
    | PFrecord (fs, r) -> transl_record fs r
  and transl_sum l rro cs r =
    let (path, decl) = lookup l in
    let cstrs = snd (List.split (Env.constructors_of_type path (Env.find_type path env))) in
    let rro = match rro with None -> None | Some (rvar, rr) -> Some (getvar rvar, transl_recref rr) in
      Fsum (path, rro, transl_cstrs (List.combine cs cstrs), transl_pref r)
  and transl_cstrs = function
    | []                  -> []
    | (ps, cstr) :: cstrs -> (cstr.cstr_tag, ("", transl_params ps)) :: transl_cstrs cstrs
  and transl_params = function
    | [] -> []
        (* pmr: need real variance here *)
    | (id, f) :: ps -> (Ident.create id, transl_pframe_rec f, Covariant) :: transl_params ps
  and transl_recref rr =
    List.map (fun rs -> List.map transl_pref rs) rr
  and transl_constr l fs r =
    let params = List.map transl_pframe_rec fs in
    let (path, decl) = lookup l in
    let _ = if List.length fs != decl.type_arity then
      raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
    let varis   = List.map translate_variance decl.type_variance in
      match decl.type_kind with 
          Type_abstract ->
            abstract_of_params path params varis (transl_pref r)
        | Type_record(fields, _, _) ->
            (* fresh_record (fresh_without_vars env) path fields *) assert false
        | Type_variant _ ->
            match List.split (Env.constructors_of_type path (Env.find_type path env)) with
              | (_, cstr :: _) -> apply_refinement (transl_pref r) 
                                    (fresh_without_vars env (snd (Ctype.instance_constructor cstr)))
              | _              -> failwith "Annotated type has no constructors!"
  and transl_record fs r =
    let ps = List.map (fun (f, s, m) -> (Ident.create s, transl_pframe_rec f, mutable_variance m)) fs in
    let path = Path.mk_ident "_anon_record" in
      record_of_params path ps (transl_pref r)
  in transl_pframe_rec pf

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
      | Tpat_var x -> (Path.Pident i, Predicate.Var (Path.Pident x)) :: subs
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
  let unsubst = List.map (Qualifier.apply qual_expr) quals in
    List.map (Predicate.apply_substs subs) unsubst

let refinement_conjuncts s qexpr res =
  C.flap (refexpr_conjuncts s qexpr) res

let refinement_predicate s qvar refn =
  Predicate.big_and (refinement_conjuncts s qvar refn)

let rec conjunct_fold cs s qual_expr f = match get_refinement f with
  | Some r -> refinement_conjuncts s qual_expr r @ cs
  | None   -> cs

let rec conjuncts s qexpr fr =
  conjunct_fold [] s qexpr fr

let predicate s qexpr fr =
  Predicate.big_and (conjuncts s qexpr fr)
