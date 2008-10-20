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

open Format
open Wellformed
module F = Frame
module Le = Lightenv
module Pat = Pattern
module P = Predicate
module T = Types
module TT = Typedtree
module TP = TheoremProver
module B = Builtins
module Q = Qualifier
module C = Common
module VM = C.PathMap 
module Cf = Clflags
module QSet = Set.Make(Q)
module NSet = Set.Make(String)
module Sol = Hashtbl.Make(C.ComparablePath)
module BS = Bstats

(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type fc_id = int option 
type subref_id = int 

module SIM = Map.Make(struct type t = subref_id let compare = compare end)

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of F.t Le.t * guard_t * F.t * F.t
  | WFFrame of F.t Le.t * F.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t 
  | Assert of Location.t 
  | Cstr of labeled_constraint

type refinement_constraint =
  | SubRef of F.refinement (* F.t *) Le.t * guard_t * F.refinement * F.simple_refinement * (subref_id option)
  | WFRef of F.t Le.t * F.simple_refinement * (subref_id option)

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

(* Unique variable to qualify when testing sat, applicability of qualifiers...
 * this is passed into the solver *)
let qual_test_var = C.qual_test_var(*Path.mk_ident "AA"*)
let qual_test_expr = P.Var qual_test_var

let is_simple_constraint c = match c with 
  | SubRef (_, _, r1, ([], F.Qvar _), _) ->
      List.for_all (function ([], ([], _)) -> true | _ -> false) r1
  | _ -> false

let is_simple_constraint2 = function 
  | SubRef (_, _, [([], ([], [k1]))], ([], F.Qvar k2), _) -> true
  | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let is_subframe_constraint = function
  SubFrame _ -> true | _ -> false

let is_wfframe_constraint = function
  WFFrame _ -> true | _ -> false

let solution_map s k = 
  C.do_catch 
    (Printf.sprintf "ERROR: solution_map couldn't find: %s" (C.path_name k))
    (Sol.find s) k  

let sref_map f r =
  let (qconsts, qvars) = F.ref_to_simples r in 
  List.map f (qconsts @ qvars)

(**************************************************************)
(**************************** Stats ***************************)
(**************************************************************)
let stat_unsat_lhs      = ref 0
let stat_wf_refines     = ref 0
let stat_sub_refines    = ref 0
let stat_simple_refines = ref 0
let stat_refines        = ref 0
let stat_imp_queries    = ref 0
let stat_valid_queries  = ref 0
let stat_matches        = ref 0
let stat_tp_refines     = ref 0
(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let guard_predicate () g = 
  P.big_and 
    (List.map 
      (fun (v,b) -> let p = P.(?.) (P.Var v) in 
         if b then p else P.Not p) 
      g)

let refinement_preds sm qexpr r =
  F.refinement_conjuncts sm qexpr r

let environment_preds sm env = 
  List.flatten (Le.maplist (fun v r -> refinement_preds sm (P.Var v) r) env)

let pprint_local_binding f ppf = function
  | (Path.Pident _ as k, v) -> 
      fprintf ppf "@[%s@ =>@ %a@],@;<0 2>" 
      (Path.unique_name k) f v
  | _ -> ()

let env_ignore_list = ["Pervasives"; "Open_"; "FP_"; "false"; "true"; "Array"; "String"; "Big"; "None"; "Some"; "Random"; "[]"; "::"]
let filter_le f e = Le.fold (fun p fr m -> if f p fr then Le.add p fr m else m) e Le.empty
let prune_background env = 
  filter_le (fun p _ -> List.for_all (fun pre -> not (C.has_prefix pre (C.path_name p))) env_ignore_list) env 

let pprint_fenv ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint f) (prune_background env); fprintf ppf "==="

let pprint_fenv_pred so ppf env =
  (* match so with
  | Some s -> P.pprint ppf (P.big_and (environment_preds (solution_map s) env))
  | _ -> *) Le.iter (fun x t -> pprint_local_binding F.pprint ppf (x, t)) env

let pprint_renv_pred f so ppf env =
  match so with
  | Some s -> P.pprint ppf (P.big_and (environment_preds (solution_map s) env))
  | _ -> Le.iter (fun x t -> pprint_local_binding F.pprint_refinement ppf (x, t)) env

let pprint ppf = function
  | SubFrame (e,g,f1,f2) ->
      if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Env)@.%a@]@." pprint_fenv e;
      if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Guard)@.%a@]@.@." P.pprint (guard_predicate () g);
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (e,f) ->
      if C.ck_olev C.ol_dump_wfs then begin
        if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Env)@.%a@]@." pprint_fenv e;
        fprintf ppf "@[|- %a@]@." F.pprint f
      end

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = 
  function
  | SubRef (renv,g,r1,sr2,io) ->
      let renv = prune_background renv in
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_renv_pred F.pprint_refinement so) renv 
      P.pprint (guard_predicate () g) 
      F.pprint_refinement r1 F.pprint_refinement (F.ref_of_simple sr2)
  | WFRef (env,sr,io) ->
      let env = prune_background env in
      C.fcprintf ppf C.ol_dump_wfs "@[%a@ Env:@ @[%a@];@\n|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_fenv_pred so) env 
      F.pprint_refinement (F.ref_of_simple sr)

let pprint_orig ppf = function
  | Loc l -> fprintf ppf "Loc@ %a" Location.print l
  | Assert l -> fprintf ppf "Assert@ %a" Location.print l
  | Cstr l -> match l.lc_id with Some l -> fprintf ppf "->@ %i" l | None -> fprintf ppf "->@ ?"

(**************************************************************)
(************* Constraint Simplification & Splitting **********) 
(**************************************************************)

let env_to_refenv env =
  Le.fold 
    (fun x f acc -> 
      match F.get_refinement f with 
      | None -> acc 
      | Some r -> Le.nil_add x r acc) 
    env Le.empty

let simplify_frame gm x f = 
  if not (Le.mem x gm) then f else
    let pos = Le.find x gm in
    match f with 
    | F.Fsum (a,b,c,[(subs,([(v1,v2,P.Iff (v3,p))],[]))]) when v3 = P.Boolexp (P.Var v2) ->
        let p' = if pos then p else P.Not p in
        F.Fsum (a,b,c,[(subs,([(v1,v2,p')],[]))])
    | _ -> f

let simplify_env env g =
  let gm = List.fold_left (fun m (x,b)  -> Le.add x b m) Le.empty g in
    Le.mapi (simplify_frame gm) env

let simplify_fc c =
  match c.lc_cstr with
  | WFFrame _ -> c 
  | SubFrame (env,g,a,b) ->
      let env' = simplify_env env g in
      {c with lc_cstr = SubFrame(env', g, a, b)}

let make_lc c fc = {lc_cstr = fc; lc_tenv = c.lc_tenv; lc_orig = Cstr c; lc_id = c.lc_id}

let lequate_cs env g c variance f1 f2 = match variance with
  | F.Invariant     -> [make_lc c (SubFrame(env,g,f1,f2)); make_lc c (SubFrame(env,g,f2,f1))]
  | F.Covariant     -> [make_lc c (SubFrame(env,g,f1,f2))]
  | F.Contravariant -> [make_lc c (SubFrame(env,g,f2,f1))]

let subst_to lfrom lto = match (lfrom, lto) with
  | (pfrom, pto) when not (Pat.same pfrom pto) -> Pat.substitution pfrom pto
  | _ -> []

let frame_env = function
    SubFrame(env, _, _, _) -> env
  | WFFrame(env, _) -> env

let make_val_env vf env = function
    SubFrame(_, g, f, f') -> SubFrame(Le.nil_add qual_test_var vf env, g, f, f') 
  | c -> c 

let patch_env env = function
    SubFrame(env', g, f, f') -> SubFrame(Le.combine env' env, g, f, f') | c -> c

let set_env env = function
    SubFrame(_, g, f, f') -> SubFrame(env, g, f, f') | c -> c

let set_labeled_constraint_env c f =
  {lc_cstr = f c.lc_cstr; lc_tenv = c.lc_tenv;
   lc_orig = c.lc_orig; lc_id = c.lc_id}

let split_sub_ref c env g r1 r2 =
  let c = set_labeled_constraint_env c (set_env env) in
  let v = (function SubFrame(_ , _, f, _) -> f | _ -> assert false) c.lc_cstr in
  sref_map (fun sr -> (v, c, SubRef(env_to_refenv env, g, r1, sr, None))) r2

let params_apply_substitutions subs ps =
  List.map (fun (i, f, v) -> (i, F.apply_subs subs f, v)) ps

let rec split_sub_params c tenv env g ps1 ps2 = match (ps1, ps2) with
  | ((i, f, v)::ps1, (i', f', _)::ps2) ->
      let (pi, pi') = (TT.Tpat_var i, TT.Tpat_var i') in
      let (env', subs) = begin match v with
        | F.Covariant | F.Invariant -> (F.env_bind env pi f, subst_to pi' pi)
        | F.Contravariant           -> (F.env_bind env pi' f', subst_to pi pi')
      end
      in lequate_cs env g c v f f' @
           split_sub_params c tenv env' g ps1 (params_apply_substitutions subs ps2)

  | ([], []) -> []
  | _ -> assert false

let bind_tags_pr (t, f) cs r env =
  let is_recvar = function
      (Some (p, _), F.Frec (p', _, _)) -> p' = p
    | _ -> false in
  let k (a, b, _) =
    (C.i2p a, if is_recvar (t, b) then f else b) in
  match F.find_tag r with
      Some tag -> (Le.addn (List.map k (F.constrs_tag_params tag cs)) env, Some tag)
    | None -> (env, None)

let bind_tags (t, f) cs r env = 
  fst (bind_tags_pr (t, f) cs r env)

let sum_subs bs cs tag =
  let paths xs = match tag with
      Some tag -> List.map C.i2p (F.params_ids (F.constrs_tag_params tag xs))
    | None -> [] in
  let mk_sub p1 p2 = (p1, P.Var p2) in
  let (ps, qs) = (paths bs, paths cs) in
    List.map2 mk_sub ps qs

let split_sub = function {lc_cstr = WFFrame _} -> assert false | {lc_cstr = SubFrame (env,g,f1,f2); lc_tenv = tenv} as c ->
  match (f1, f2) with
  | (f1, f2) when F.is_shape f1 && F.is_shape f2 ->
      ([], [])
  | (f1, f2) when f1 = f2 ->
      ([], [])
  | (F.Farrow (p1, f1, f1'), F.Farrow (p2, f2, f2')) ->
      let subs = if not (Pat.same p1 p2) then subst_to p1 p2 else [] in
      let env' = F.env_bind env p2 f2 in
      let f1'  = F.apply_subs subs f1' in
        (lequate_cs env g c F.Covariant f2 f1 @ lequate_cs env' g c F.Covariant f1' f2', [])
  | (F.Fvar (_, _, s, r1), F.Fvar (_, _, s', r2)) ->
      ([], split_sub_ref c env g r1 r2)
  | (F.Frec _, F.Frec _) ->
      ([], [])
  | (F.Fsum(_, None, cs1, r1), F.Fsum(_, None, cs2, r2)) ->
      let (penv, tag) = bind_tags_pr (None, f1) cs1 r1 env in
      let subs        = sum_subs cs1 cs2 tag in
      (C.flap2 (split_sub_params c tenv env g) (List.map F.constr_params cs1) (List.map F.constr_params cs2),
       split_sub_ref c penv g r1 (List.map (F.refexpr_apply_subs subs) r2))
  | (F.Fsum(_, Some (_, rr1), cs1, r1), F.Fsum(_, Some (_, rr2), cs2, r2)) ->
      let (shp1, shp2) = (F.shape f1, F.shape f2) in
      let (f1, f2)     = (F.replace_recvar (F.apply_recref rr1 f1) shp1, 
                          F.replace_recvar (F.apply_recref rr2 f2) shp2) in
        (lequate_cs env g c F.Covariant f1 f2, [])
  | (F.Fabstract(_, ps1, r1), F.Fabstract(_, ps2, r2)) ->
      (split_sub_params c tenv env g ps1 ps2, split_sub_ref c env g r1 r2)
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf_ref f c env r =
  sref_map (fun sr -> (f, c, WFRef(Le.aliasing_add qual_test_var f env, sr, None))) r

let make_wff c tenv env f =
  {lc_cstr = WFFrame (env, f); lc_tenv = tenv; lc_orig = Cstr c; lc_id = None}

let rec split_wf_params c tenv env ps =
  let wf_param (wfs, env) (i, f, _) =
    (make_wff c tenv env f :: wfs, Le.add (Path.Pident i) f env)
  in fst (List.fold_left wf_param ([], env) ps)

let split_wf = function {lc_cstr = SubFrame _} -> assert false | {lc_cstr = WFFrame (env,f); lc_tenv = tenv} as c ->
  match f with
  | f when F.is_shape f ->
      ([], [])
  | F.Fsum (_, (None as t), cs, r) ->
      (C.flap (split_wf_params c tenv env) (List.map F.constr_params cs),
       split_wf_ref f c (bind_tags (t, f) cs r env) r)
  | F.Fsum (p, (Some (_, rr) as t), cs, r) ->
      (* This deviates from the paper's cons rule because we instantiate qualifiers using
         the split constraints.  This would result in qualifiers with idents that don't
         actually appear in the program or tuple labels appearing in the recursive
         refinements as a result of rho-application's renaming. *)
      let shp     = F.shape f in
      let (f', f'') = (F.replace_recvar f shp, F.apply_recref rr shp) in
        ([make_wff c tenv env (F.apply_refinement F.empty_refinement f'); make_wff c tenv env f''], split_wf_ref f c (bind_tags (t, f) cs r env) r)
  | F.Fabstract (_, ps, r) ->
      (split_wf_params c tenv env ps, split_wf_ref f c env r)
  | F.Farrow (p, f, f') ->
      ([make_wff c tenv env f; make_wff c tenv (F.env_bind env p f) f'], [])
  | F.Fvar (_, _, s, r) ->
      ([], split_wf_ref f c env r)
  | F.Frec _ ->
      ([], [])

let split cs =
  assert (List.for_all (fun c -> None <> c.lc_id) cs);
  C.expand (fun c -> 
      match c.lc_cstr with 
      | SubFrame _ -> split_sub c 
      | WFFrame _ -> split_wf c) cs []

(**************************************************************)
(********************* Constraint Indexing ********************) 
(**************************************************************)

module WH = 
  Heap.Functional(struct 
      type t = subref_id * int * (int * bool * fc_id)
      let compare (_,ts,(i,j,k)) (_,ts',(i',j',k')) =
        if i <> i' then compare i i' else
          if ts <> ts' then -(compare ts ts') else
            if j <> j' then compare j j' else 
              compare k' k
    end)

type ref_index = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;   (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,r,_,_) ->
  Le.fold (fun _ f l -> F.refinement_qvars f @ l) env (F.refinement_qvars r)

let rhs_k = function
  | SubRef (_,_,_,(_, F.Qvar k),_) -> Some k
  | _ -> None

let wf_k = function
  | WFRef (_, (_, F.Qvar k), _) -> Some k
  | _ -> None

let ref_k c =
  match (rhs_k c, wf_k c) with
  | (Some k, None)
  | (None, Some k) -> Some k
  | _ -> None

let ref_id = function
  | WFRef (_, (_, _), Some id)
  | SubRef (_,_,_,(_, _), Some id) -> id
  | _ -> -1

let print_scc_edge rm (u,v) = 
  let (scc_u,_,_) = SIM.find u rm in
  let (scc_v,_,_) = SIM.find v rm in
  let tag = if scc_u > scc_v then "entry" else "inner" in
  C.cprintf C.ol_solve "@[SCC@ edge@ %d@ (%s)@ %d@ ====> %d@\n@]" scc_v tag u v

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    BS.time "step 1"
    (SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm) VM.empty in
  let (dm,deps) = 
    BS.time "step 2"
    (SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, (List.rev_append deps' deps)))
      cm) (SIM.empty,[]) in
  let flabel i = C.io_to_string ((SIM.find i om).lc_id) in
  let rm = 
    let rank = BS.time "scc rank" (C.scc_rank flabel) deps in
    BS.time "step 2"
    (List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = (SIM.find id om).lc_id in
        SIM.add id (r,b,fci) rm)
      SIM.empty) rank in
  (dm,rm)

let fresh_refc = 
  let i = ref 0 in
  fun c -> 
    let i' = incr i; !i in
    match c with  
    | WFRef (env,r,None) -> WFRef (env,r,Some i')
    | SubRef (env,g,r1,r2,None) -> SubRef (env,g,r1,r2,Some i')
    | _ -> assert false

(* API *)
let make_ref_index ocs = 
  let ics = List.map (fun (o,c) -> (o,fresh_refc c)) ocs in
  let (om,cm) = 
    List.fold_left 
      (fun (om,cm) (o,c) ->
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = BS.time "make rank map" (make_rank_map om) cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig

let get_ref_fenv sri c =
  (function SubFrame (a, _, _, _) | WFFrame (a, _) -> a) (get_ref_orig sri c).lc_cstr

(* API *)
let get_ref_deps sri c =
  let is' = try SIM.find (get_ref_id c) sri.depm with Not_found -> [] in
  List.map (get_ref_constraint sri) is'

(* API *)
let get_ref_constraints sri = 
  SIM.fold (fun _ c cs -> c::cs) sri.cnst [] 

(* API *)
let iter_ref_constraints sri f = 
  SIM.iter (fun _ c -> f c) sri.cnst

let iter_ref_origs sri f =
  SIM.iter (fun i c -> f i c) sri.orig

let sort_iter_ref_constraints sri f = 
  let rids  = SIM.fold (fun id (r,_,_) ac -> (id,r)::ac) sri.rank [] in
  let rids' = List.sort (fun x y -> compare (snd x) (snd y)) rids in 
  List.iter (fun (id,_) -> f (SIM.find id sri.cnst)) rids' 


(* API *)
let push_worklist =
  let timestamp = ref 0 in
  fun sri w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = get_ref_id c in
        if Hashtbl.mem sri.pend id then w else 
          (C.cprintf C.ol_solve "@[Pushing@ %d at %d@\n@]" id !timestamp; 
           Hashtbl.replace sri.pend id (); 
           WH.add (id,!timestamp,get_ref_rank sri c) w))
      w cs

(* API *)
let pop_worklist sri w =
  try 
    let (id, _, _) = WH.maximum w in
    let _ = Hashtbl.remove sri.pend id in
    (Some (get_ref_constraint sri id), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let make_initial_worklist sri =
  let cs = List.filter is_subref_constraint (get_ref_constraints sri) in
  push_worklist sri WH.empty cs 

(**************************************************************)
(************************** Refinement ************************)
(**************************************************************)

module PM = Map.Make(struct type t = P.t let compare = compare end)

let close_over_env env s ps =
  let rec close_rec clo = function
      | [] -> clo
      | ((P.Atom (P.Var x, P.Eq, P.Var y)) as p)::ps ->
          let tvar =
            if Path.same x qual_test_var then Some y else 
              if Path.same y qual_test_var then Some x else None in
          (match tvar with None -> close_rec (p :: clo) ps | Some t ->
            let ps' = F.conjuncts s qual_test_expr (Le.find t env) in
            close_rec (p :: clo) (ps'@ps))
      | p::ps -> close_rec (p :: clo) ps in
  close_rec [] ps 

let refine_sol_update s k qs qs' = 
  (* C.cprintf C.ol_refine "@[%s : %d --> %d %a@.@]" 
    (Path.unique_name k) (List.length qs) (List.length qs')
    (Oprint.print_list Q.pprint C.space) qs'; *)
  BS.time "sol replace" (Sol.replace s k) qs';
  not (C.same_length qs qs')

let refine_simple s r1 k2 =
  let k1s  = C.flap (fun (_,(_,ks)) -> ks) r1 in
  let q1s  = C.flap (Sol.find s) k1s in
  let q1s  = List.fold_left (fun qs q -> QSet.add q qs) QSet.empty q1s in
  let q2s  = Sol.find s k2 in
  let q2s' = List.filter (fun q -> QSet.mem q q1s) q2s in
  refine_sol_update s k2 q2s q2s'

let pfalse = P.Not (P.True)

let falses = [pfalse; P.Atom (P.PInt 0, P.Eq, P.PInt 1); P.Atom (P.PInt 1, P.Eq, P.PInt 0)]

let qual_wf sm env subs q =
  BS.time "qual_wf" 
  (refinement_well_formed env sm (F.mk_refinement subs [q] [])) qual_test_expr

let lhs_preds sm env g r1 =
  let gp    = guard_predicate () g in
  let envps = environment_preds sm env in
  let r1ps  = refinement_preds  sm qual_test_expr r1 in
  envps @ (gp :: r1ps) 

let rhs_cands sm subs k = 
  List.map 
    (fun q -> 
      let r = F.mk_refinement subs [q] [] in
      (q,F.refinement_predicate sm qual_test_expr r))
    (sm k) 

let check_tp senv lhs_ps x2 = 
  let dump s p = 
    let p = List.map (fun p -> P.big_and (List.filter (function P.Atom(p, P.Eq, p') when p = p' -> false | _ -> true) (P.conjuncts p))) p in
    let p = List.filter (function P.True -> false | _ -> true) p in
    C.cprintf C.ol_dump_prover "@[%s:@ %a@]@." s (C.pprint_list " " P.pprint) p in
  let dump s p = if C.ck_olev C.ol_dump_prover then dump s p else () in
  let _ = dump "Assert" lhs_ps in
  let _ = if C.ck_olev C.ol_dump_prover then dump "Ck" (snd (List.split x2)) in
  let rv = 
    try
      TP.set_and_filter senv lhs_ps x2
    with Failure x -> printf "%a@." pprint_fenv senv; raise (Failure x) in
  let _ = if C.ck_olev C.ol_dump_prover then dump "OK" (snd (List.split rv)) in
  incr stat_tp_refines;
  stat_imp_queries   := !stat_imp_queries + (List.length x2);
  stat_valid_queries := !stat_valid_queries + (List.length rv); rv

let check_tp senv lhs_ps x2 =
  if C.empty_list x2 then (incr stat_tp_refines; []) else BS.time "check_tp" (check_tp senv lhs_ps) x2 

let bound_in_env senv p =
  List.for_all (fun x -> Le.mem x senv) (P.vars p)

(*
let check_env_bindings senv lhs_ps rhs_ps =
  let chl = List.for_all (bound_in_env senv) lhs_ps in
  let chr = List.for_all (bound_in_env senv) rhs_ps in
  if not (chl && chr) then
    (printf "@[lhs: %a@]@.@." P.pprint (P.big_and lhs_ps);
     printf "@[rhs: %a@]@.@." P.pprint (P.big_and rhs_ps);
     Printf.printf "bad env bindings (l=%b, r=%b)!!! " chl chr)
*)

let refine_tp senv s env g r1 sub2s k2 =
  let sm = solution_map s in
  let lhs_ps  = lhs_preds sm env g r1 in
  let rhs_qps = rhs_cands sm sub2s k2 in
(*  let _       = check_env_bindings senv lhs_ps (List.map snd rhs_qps) in *)
  let rhs_qps' =
    if List.exists (fun p -> List.mem p falses) lhs_ps 
    then (stat_matches := !stat_matches + (List.length rhs_qps); rhs_qps) 
    else
      let rhs_qps = List.filter (fun (_,p) -> not (List.mem p falses)) rhs_qps in
      let lhsm    = List.fold_left (fun pm p -> PM.add p true pm) PM.empty lhs_ps in
      let (x1,x2) = List.partition (fun (_,p) -> PM.mem p lhsm) rhs_qps in
      let _       = stat_matches := !stat_matches + (List.length x1) in 
      let x2      = List.filter (fun (_,p) -> bound_in_env senv p) x2 in
      match x2 with [] -> x1 | _ -> x1 @ (check_tp senv lhs_ps x2) in
  refine_sol_update s k2 rhs_qps (List.map fst rhs_qps') 

let refine sri s c =
  (incr stat_refines;
   match c with 
   | SubRef _ -> incr stat_sub_refines 
   | WFRef _ -> incr stat_wf_refines); 
  let sm = solution_map s in
  match c with
  | SubRef (_, _, r1, ([], F.Qvar k2), _)
    when is_simple_constraint c && not (!Cf.no_simple || !Cf.verify_simple) ->
      incr stat_simple_refines; 
      refine_simple s r1 k2
  | SubRef (_, _, _, (_, F.Qvar k2), _) when C.empty_list (sm k2) ->
      false
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      refine_tp (get_ref_fenv sri c) s env g r1 sub2s k2 
  | WFRef (env, (subs, F.Qvar k), Some id) ->
      let qs  = solution_map s k in
      let _   = if C.ck_olev C.ol_dump_prover then printf "@.@.@[WF: %s@]@." (Path.unique_name k) in
      let _   = if C.ck_olev C.ol_dump_prover then printf "@[(Env)@ %a@]@." pprint_fenv env in 
      let qs' = BS.time "filter wf" (List.filter (qual_wf sm env subs)) qs in
      let _   = if C.ck_olev C.ol_dump_prover then List.iter (fun q -> printf "%a" Qualifier.pprint q) qs in
      let _   = if C.ck_olev C.ol_dump_prover then printf "@.@." in
      let _   = if C.ck_olev C.ol_dump_prover then List.iter (fun q -> printf "%a" Qualifier.pprint q) qs' in
      refine_sol_update s k qs qs'
  | _ -> false


(**************************************************************)
(********************** Constraint Satisfaction ***************)
(**************************************************************)

let sat sri s c = 
  match c with
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      not (refine_tp (get_ref_fenv sri c) s env g r1 sub2s k2)
  | SubRef (env, g, r1, sr2, _) as c ->
      let sm     = solution_map s in
      let lhs_ps = lhs_preds sm env g r1 in
      let rhs    = F.refinement_predicate sm qual_test_expr (F.ref_of_simple sr2) in
      1 = List.length (check_tp (get_ref_fenv sri c) lhs_ps [(0,rhs)])
  | WFRef (env,(subs, F.Qvar k), _) ->
      true 
  | _ -> true

let unsat_constraints sri s =
  C.map_partial
    (fun c -> if sat sri s c then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(**************************************************************)
(******************** Qualifier Instantiation *****************)
(**************************************************************)

module LEnv : Graph.Sig.COMPARABLE with type t = Frame.t Le.t =
struct
   type t = Frame.t Le.t
   let compare = compare
   let hash e = Hashtbl.hash (Le.setstring e)
   let equal = (==)
end

module EG = Graph.Persistent.Digraph.Concrete(LEnv)

let memo = Hashtbl.create 1000

let add_path_set vars m path =
  match Path.ident_name path with
  | Some name when NSet.mem name vars ->
      let rest = try C.StringMap.find name m with Not_found -> [] in C.StringMap.add name (path :: rest) m
  | _ -> m

let rec find_max_containing_env envg env = match EG.succ envg (Le.unalias env) with
  | []     -> env
  | e :: _ -> find_max_containing_env envg e

let instantiate_in_vm vm q =
  List.fold_left (C.flip QSet.add) QSet.empty (Q.instantiate_about vm q)

let instantiate_quals_in_env envg qs =
  let qpaths = List.fold_left (fun xs x -> List.fold_left (C.flip NSet.add) xs (Q.vars x)) NSet.empty qs in
    fun env ->
      let env  = find_max_containing_env envg env in
      let estr = Le.setstring env in
        try Hashtbl.find memo estr with Not_found ->
          let vm  = List.fold_left (add_path_set qpaths) C.StringMap.empty (Le.domain env) in
          let q   = List.fold_left (fun qset q -> QSet.union (instantiate_in_vm vm q) qset) QSet.empty qs in
          let els = QSet.elements q in
          let _   = Hashtbl.replace memo estr els in
            els

let rec add_env envg (Le.Env (_, origin) as e) = match origin with
  | Le.Parent parent -> add_env (EG.add_edge envg parent e) parent
  | Le.Alias  alias  -> add_env envg alias
  | Le.Nil           -> EG.add_vertex envg e

let make_envg envs =
  List.fold_left add_env EG.empty envs

let constraint_env (_, c) =
  match c with | SubRef (_, _, _, _, _) -> Le.empty | WFRef (e, _, _) -> e

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from all environments. *)
let instantiate_per_environment cs qs =
  let envs = BS.time "cenv" (List.rev_map constraint_env) cs in
  let envg = BS.time "envg" make_envg envs in
  BS.time "instquals" (List.rev_map (instantiate_quals_in_env envg qs)) envs

(**************************************************************)
(************************ Initial Solution ********************)
(**************************************************************)

(* If a variable only ever appears on the left hand side, the variable is
 * unconstrained; this generally indicates an uncalled function.
 * When we have such an unconstrained variable, we simply say we don't
 * know anything about its value.  For uncalled functions, this will give
 * us the type which makes the least assumptions about the input. *)

let strip_origins cs = snd (List.split cs)
(*
let make_initial_solution cs =
  let s0   = Sol.create 37 in
  let rhst = Sol.create 37 in
  List.iter 
    (function (SubRef (_, _, _, (_, F.Qvar k), _), _) -> 
      Sol.replace rhst k true
     | _ -> ()) cs;
  List.iter
    (function (WFRef (_, (_, F.Qvar k), _), qs) ->
      let iqs = if Sol.mem rhst k || !Cf.minsol then qs else [] in
      List.iter (Sol.add s0 k) iqs
     | _ -> ()) cs;
  let s    = Sol.create 37 in
  Sol.iter 
    (fun k _ -> 
      let qs  = Sol.find_all s0 k in
      let qs' = C.sort_and_compact qs in
      Sol.replace s k qs') s0;
  s
*)

let formals = ref []
let is_formal q = List.mem q !formals
let formals_addn qs = formals := List.rev_append qs !formals
  
let filter_wfs cs = List.filter (fun (r, _) -> match r with WFRef(_, _, _) -> true | _ -> false) cs
let filter_subs cs = List.filter (fun (r, _) -> match r with SubRef(_, _, _, _, _) -> true | _ -> false) cs
type solmode = WFS | LHS | RHS

let make_initial_solution cs =
  let s    = Sol.create 37 in
  let addrv qs = function
    | (F.Qconst _, _) -> ()
    | (F.Qvar k, LHS) -> if not (Sol.mem s k) then (if not (!Cf.minsol) && is_formal k then Sol.replace s k [] else Sol.replace s k qs)
    | (F.Qvar k, RHS) -> Sol.replace s k qs
    | (F.Qvar k, WFS) -> if Sol.find s k != [] then Sol.replace s k qs in
  let ga (c, q) = match c with
    | SubRef (_, _, r1, (_, qe2), _) ->
        List.iter (fun qv -> addrv q (F.Qvar qv, LHS)) (F.refinement_qvars r1); addrv q (qe2, RHS)
    | WFRef (_, (_, qe), _) -> addrv q (qe, LHS); addrv q (qe, WFS) in
  let wfs  = filter_wfs cs in
  let subs = filter_subs cs in
  List.iter ga subs; List.iter ga wfs; s

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)
 
let dump_ref_constraints sri =
  if !Cf.dump_ref_constraints then begin
    printf "@[Refinement Constraints@.@\n@]";
    iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c);
    printf "@[SCC Ranked Refinement Constraints@.@\n@]";
    sort_iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c);
    (*printf "@[Refinement Constraint Origins@.@.@]";
    iter_ref_origs sri 
      (fun i c -> printf "@[o(%i)@]@." i; Le.iter (fun p _ -> printf "@[%s@]@." (Path.unique_name p)) (frame_env c.lc_cstr))*)  
  end

let dump_ref_vars sri =
  if !Cf.dump_ref_vars then
  (printf "@[Refinement Constraint Vars@.@\n@]";
  iter_ref_constraints sri (fun c -> printf "@[(%d)@ %s@.@]" (ref_id c) 
    (match (ref_k c) with Some k -> Path.unique_name k | None -> "None")))
   
let dump_constraints cs =
  if !Cf.dump_constraints then begin
    printf "******************Frame Constraints****************@.@.";
    let index = ref 0 in
    List.iter (fun {lc_cstr = c; lc_orig = d} -> incr index; printf "@[(%d)(%a) %a@]@.@." !index pprint_orig d pprint c) cs;
    printf "@[*************************************************@]@.@.";
  end

let dump_solution_stats s = 
  if C.ck_olev C.ol_solve_stats then
    let kn  = Sol.length s in
    let (sum, max, min) =   
      (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
      Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
      Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
    C.cprintf C.ol_solve_stats "@[Quals:@\n\tTotal:@ %d@\n\tAvg:@ %f@\n\tMax:@ %d@\n\tMin:@ %d@\n@\n@]"
    sum ((float_of_int sum) /. (float_of_int kn)) max min;
    print_flush ()
  else ()
  
let dump_unsplit cs =
  let cs = if C.ck_olev C.ol_solve_stats then List.rev_map (fun c -> c.lc_cstr) cs else [] in
  let cc f = List.length (List.filter f cs) in
  let (wf, sub) = (cc is_wfframe_constraint, cc is_subframe_constraint) in
  C.cprintf C.ol_solve_stats "@.@[unsplit@ constraints:@ %d@ total@ %d@ wf@ %d@ sub@]@.@." (List.length cs) wf sub

let dump_solving sri s step =
  if step = 0 then 
    let cs   = get_ref_constraints sri in 
    let kn   = Sol.length s in
    let wcn  = List.length (List.filter is_wfref_constraint cs) in
    let rcn  = List.length (List.filter is_subref_constraint cs) in
    let scn  = List.length (List.filter is_simple_constraint cs) in
    let scn2 = List.length (List.filter is_simple_constraint2 cs) in
    (dump_ref_vars sri;
     dump_ref_constraints sri;
     C.cprintf C.ol_solve_stats "@[%d@ variables@\n@\n@]" kn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ wf@ constraints@\n@\n@]" wcn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ subtyping@ constraints@\n@\n@]" rcn;
     C.cprintf C.ol_solve_stats "@[%d@ simple@ subtyping@ constraints@\n@\n@]" scn;
     C.cprintf C.ol_solve_stats "@[%d@ simple2@ subtyping@ constraints@\n@\n@]" scn2;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (C.cprintf C.ol_solve_stats 
      "@[Refine Iterations: %d@ total (= wf=%d + su=%d) sub includes si=%d tp=%d unsatLHS=%d)\n@\n@]"
      !stat_refines !stat_wf_refines  !stat_sub_refines !stat_simple_refines !stat_tp_refines !stat_unsat_lhs;
     C.cprintf C.ol_solve_stats "@[Implication Queries:@ %d@ match;@ %d@ to@ TP@ (%d@ valid)@]@.@." 
       !stat_matches !stat_imp_queries !stat_valid_queries;
     if C.ck_olev C.ol_solve_stats then TP.print_stats std_formatter () else ();
     dump_solution_stats s;
     flush stdout)

let dump_solution s =
  if C.ck_olev C.ol_solve then
    Sol.iter (fun p r -> C.cprintf C.ol_solve "@[%s: %a@]@."
              (Path.unique_name p) (Oprint.print_list Q.pprint C.space) r) s
  else ()

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

let rec solve_sub sri s w = 
  (if !stat_refines mod 100 = 0 
   then C.cprintf C.ol_solve "@[num@ refines@ =@ %d@\n@]" !stat_refines);
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let (r,b,fci) = get_ref_rank sri c in
    let _ = C.cprintf C.ol_solve "@.@[Refining@ %d@ at iter@ %d in@ scc@ (%d,%b,%s):@]@."
            (get_ref_id c) !stat_refines r b (C.io_to_string fci) in
    let _ = if C.ck_olev C.ol_insane then dump_solution s in
    let w' = if BS.time "refine" (refine sri s) c 
             then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub sri s w'

let solve_wf sri s =
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine sri s c) | _ -> ())

let solve qs cs = 
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let _  = dump_constraints cs in
  let _  = dump_unsplit cs in
  let cs = BS.time "splitting constraints" split cs in
  let max_env = BS.time "max env" (List.fold_left 
    (fun env (v, c', c) -> match c with WFRef (e, _, _) -> Le.combine e env | SubRef _ -> Le.combine (frame_env c'.lc_cstr) env) Le.empty) cs in
  let _ = let x = ref 0 in Le.iter (fun _ _ -> incr x) max_env; printf "%i@." !x in
  let cs = BS.time "inject val var" (List.map (fun (v, c, cstr) -> (set_labeled_constraint_env c (make_val_env v max_env), cstr))) cs in
  (* let cs = if !Cf.esimple then 
               BS.time "e-simplification" (List.map esimple) cs else cs in *)
  let qs = BS.time "instantiating quals" (instantiate_per_environment cs) qs in
  let qs = BS.time "pruning quals" (List.map (fun qs -> List.filter Qualifier.may_not_be_tautology qs)) qs in
  let _ = Hashtbl.clear memo in
  let sri = BS.time "making ref index" make_ref_index cs in
  let s = make_initial_solution (List.combine (strip_origins cs) qs) in
  let _ = dump_solution s in
  let _ = dump_solving sri s 0 in
  let _ = BS.time "solving wfs" (solve_wf sri) s in
  let _ = C.cprintf C.ol_solve "@[AFTER@ WF@]@." in
  let _ = dump_solving sri s 1 in
  let _ = dump_solution s in
  let w = make_initial_worklist sri in
  let _ = BS.time "solving sub" (solve_sub sri s) w in
  let _ = dump_solving sri s 2 in
  let _ = dump_solution s in
  let _ = TP.reset () in
  let unsat = BS.time "testing solution" (unsat_constraints sri) s in
  (if List.length unsat > 0 then 
    C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
    List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat);
  (solution_map s, List.map snd unsat)
