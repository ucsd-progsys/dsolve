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

module C = Common
module VM = C.PathMap 
module Sol = Hashtbl.Make(C.ComparablePath)
module Cf = Clflags

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
  | SubRef of F.t Le.t * guard_t * F.refinement * F.simple_refinement * (subref_id option)
  | WFRef of F.t Le.t * F.simple_refinement * (subref_id option)

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

(* Unique variable to qualify when testing sat, applicability of qualifiers...
 * this is passed into the solver *)
let qual_test_var = Path.mk_ident "AA"
let qual_test_expr = P.Var qual_test_var

let is_simple_constraint = function 
  | SubRef (_, _, r1, ([], F.Qvar _), _) ->
      List.for_all (function ([], ([], _)) -> true | _ -> false) r1
  | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let solution_map s k = 
  C.do_catch 
    (Printf.sprintf "ERROR: solution_map couldn't find: %s" (C.path_name k))
    (Sol.find s) k  

let sref_map f r =
  let (qconsts, qvars) = F.ref_to_simples r in List.map f (qconsts @ qvars)

(**************************************************************)
(**************************** Stats ***************************)
(**************************************************************)
let stat_wf_refines = ref 0
let stat_sub_refines = ref 0
let stat_simple_refines = ref 0
let stat_refines = ref 0
let stat_imp_queries = ref 0
let stat_valid_imp_queries = ref 0
let stat_matches = ref 0 

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let guard_predicate () g = 
  P.big_and 
    (List.map 
      (fun (v,b) -> 
         let p = P.equals (B.tag (P.Var v), P.PInt 1) in
         if b then p else P.Not p) 
      g)

let environment_predicate sm env =
  P.big_and (Le.maplist (fun v -> F.predicate sm (P.Var v)) env)

let pprint_local_binding ppf = function
  | (Path.Pident _ as k, v) -> fprintf ppf "@[%s@;=>@;<1 2>%a@],@;<1 0>" (Path.unique_name k) F.pprint v
  | _ -> ()

let pprint_env_pred so ppf env =
  match so with
  | Some s -> P.pprint ppf (environment_predicate (solution_map s) env)
  | _ -> Le.iter (fun x t -> pprint_local_binding ppf (x, t)) env

let pprint ppf = function
  | SubFrame (_,_,f1,f2) ->
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (_,f) ->
      F.pprint ppf f

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = function
  | SubRef (env,g,r1,sr2,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@;<1 0>|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_env_pred so) env P.pprint (guard_predicate () g) 
      F.pprint_refinement r1 F.pprint_refinement (F.ref_of_simple sr2)
  | WFRef (env,sr,io) ->
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_env_pred so) env F.pprint_refinement (F.ref_of_simple sr)


(**************************************************************)
(************* Constraint Simplification & Splitting **********) 
(**************************************************************)

let simplify_frame gm x f = 
  if not (Le.mem x gm) then f else
    let pos = Le.find x gm in
    match f with 
    | F.Fsum (a,b,c,[(subs,([(v1,v2,P.Iff (v3,p))],[]))]) when v3 = B.tag (P.Var v2) ->
        let p' = if pos then p else P.Not p in
        F.Fsum (a,b,c,[(subs,([(v1,v2,p')],[]))])
    | _ -> f

let simplify_env env g =
  let gm = List.fold_left (fun m (x,b)  -> Le.add x b m) Le.empty g in
  Le.fold 
    (fun x f env' ->
      match f with | F.Fvar _ | F.Fsum _ | F.Fabstract _ ->
        Le.add x (simplify_frame gm x f) env' 
      | _ -> env')
    env Le.empty

let simplify_fc c =
  match c.lc_cstr with
  | WFFrame _ -> c 
  | SubFrame (env,g,a,b) ->
      let env' = simplify_env env g in
      (* let _ = printf "@[Simplify env to: %a@.@]" (pprint_env_pred None) env' in *)
      {c with lc_cstr = SubFrame(env', g, a, b)}

(* Notes:
  * 2. pmr: because we were only filtering through invariant types 
  * anyway, we might as well just use invariants until we start 
  * getting problems from it --- for now, it's too much trouble 
  * to work around all the BigArray stuff
  *)

let make_lc c fc = {lc_cstr = fc; lc_tenv = c.lc_tenv; lc_orig = Cstr c; lc_id = c.lc_id}

let lequate_cs env g c variance f1 f2 = match variance with
  | F.Invariant -> [make_lc c (SubFrame(env,g,f1,f2)); make_lc c (SubFrame(env,g,f2,f1))]
  | F.Covariant -> [make_lc c (SubFrame(env,g,f1,f2))]
  | F.Contravariant -> [make_lc c (SubFrame(env,g,f2,f1))]

let subst_to lfrom lto = match (lfrom, lto) with
  | (pfrom, pto) when not (Pat.same pfrom pto) -> Pat.substitution pfrom pto
  | _ -> []

let split_sub_ref c env g r1 r2 =
  sref_map (fun sr -> (Cstr c, SubRef(env, g, r1, sr, None))) r2

let params_apply_substitutions subs ps =
  List.map (fun (i, f, v) -> (i, F.apply_subs subs f, v)) ps

let rec split_sub_params c tenv env g ps1 ps2 = match (ps1, ps2) with
  | ((i, f, v)::ps1, (i', f', _)::ps2) ->
      let (pi, pi') = (TT.Tpat_var i, TT.Tpat_var i') in
      let (env', subs) = begin match v with
        | F.Covariant | F.Invariant -> (F.env_bind tenv env pi f, subst_to pi' pi)
        | F.Contravariant           -> (F.env_bind tenv env pi' f', subst_to pi pi')
      end
      in lequate_cs env g c v f f' @
           split_sub_params c tenv env' g ps1 (params_apply_substitutions subs ps2)

  | ([], []) -> []
  | _ -> assert false

let resolve_extend_env tenv env f l1 l2 = match (l1, l2) with
  | (Some p, _) | (_, Some p) -> F.env_bind tenv env p f
  | _ -> env

let no_recrefs = function
  | (None, None)                   -> true
  | (Some (_, rr1), Some (_, rr2)) -> F.recref_is_empty rr1 && F.recref_is_empty rr2
  | _                              -> false

let bind_tags_pr (t, f) cs r env =
  let is_recvar = function
      (Some (p, _), F.Frec (p', _, _)) -> p' = p
    | _ -> false in
  let k (a, b, _) =
    (C.i2p a, if is_recvar (t, b) then f else b) in
  match F.find_tag r with
      Some tag -> (Le.addn (List.map k (List.assoc tag cs)) env, Some tag)
    | None -> (env, None)

let bind_tags (t, f) cs r env = 
  fst (bind_tags_pr (t, f) cs r env)

let sum_subs bs cs tag =
  let paths xs = match tag with
      Some tag -> List.map C.i2p (F.params_ids (List.assoc tag xs))
    | None -> [] in
  let mk_sub p1 p2 = (p1, P.Var p2) in
  let (ps, qs) = (paths bs, paths cs) in
    List.map2 mk_sub ps qs

let app_subs ss (oss, qks) =
  (ss @ oss, qks)

let split_sub = function {lc_cstr = WFFrame _} -> assert false | {lc_cstr = SubFrame (env,g,f1,f2); lc_tenv = tenv} as c ->
  match (f1, f2) with
  | (F.Farrow (l1, f1, f1'), F.Farrow (l2, f2, f2')) ->
      let subs = match (l1, l2) with (Some p1, Some p2) when not (Pat.same p1 p2) -> subst_to p2 p1 | _ -> [] in
      let env' = resolve_extend_env tenv env f2 l1 l2 in
      let f1' = F.apply_subs subs f1' in
      ((lequate_cs env g c F.Covariant f2 f1) @ (lequate_cs env' g c F.Covariant f1' f2'), [])
  | (F.Fvar (_, _, r1), F.Fvar (_, _, r2)) ->
      ([], split_sub_ref c env g r1 r2)
  | (F.Frec _, F.Frec _) ->
      ([], [])
  | (F.Funknown, F.Funknown) ->
      ([],[]) 
  | (F.Fsum(_, t1, cs1, r1), F.Fsum(_, t2, cs2, r2)) when no_recrefs (t1, t2) ->  (* 2 *)
      let (penv, tag) = bind_tags_pr (None, f1) cs1 r1 env in
      let subs = sum_subs cs1 cs2 tag in
      (C.flap2 (fun (_, ps1) (_, ps2) -> split_sub_params c tenv env g ps1 ps2) cs1 cs2,
       split_sub_ref c penv g r1 (List.map (app_subs subs) r2))
  | (F.Fsum(_, Some (_, rr1), cs1, r1), F.Fsum(_, Some (_, rr2), cs2, r2)) ->
      let (shp1, shp2) = (F.shape f1, F.shape f2) in
      let (f1, f2) = (F.replace_recvar (F.apply_recref rr1 f1) shp1, F.replace_recvar (F.apply_recref rr2 f2) shp2) in
      let (penv, tag) = bind_tags_pr (None, f1) cs1 r1 env in
      let subs = sum_subs cs1 cs2 tag in
      (lequate_cs env g c F.Covariant f1 f2,
       split_sub_ref c penv g r1 (List.map (app_subs subs) r2))
  | (F.Fabstract(_, ps1, r1), F.Fabstract(_, ps2, r2)) ->
      (split_sub_params c tenv env g ps1 ps2, split_sub_ref c env g r1 r2)
  | (_,_) -> 
      (printf "@[Can't@ split:@ %a@ <:@ %a@]" F.pprint f1 F.pprint f2; 
       assert false)

let split_wf_ref f c env r =
  sref_map (fun sr -> (Cstr c, WFRef(Le.add qual_test_var f env, sr, None))) r

let make_wff c tenv env f =
  {lc_cstr = WFFrame (env, f); lc_tenv = tenv; lc_orig = Cstr c; lc_id = None}

let rec split_wf_params c tenv env ps =
  let wf_param (wfs, env) (i, f, _) =
    (make_wff c tenv env f :: wfs, Le.add (Path.Pident i) f env)
  in fst (List.fold_left wf_param ([], env) ps)

let split_wf = function {lc_cstr = SubFrame _} -> assert false | {lc_cstr = WFFrame (env,f); lc_tenv = tenv} as c ->
  match f with
  | F.Fsum (p, (Some (rp, rr) as t), cs, r) when not (F.recref_is_empty rr) ->
      (* This deviates from the paper's cons rule because we instantiate qualifiers using
         the split constraints.  This would result in qualifiers with idents that don't
         actually appear in the program or tuple labels appearing in the recursive
         refinements as a result of rho-application's renaming. *)
      let shp     = F.shape f in
      let (f', f'') = (F.replace_recvar f shp, F.apply_recref rr shp) in
        ([make_wff c tenv env (F.apply_refinement F.empty_refinement f'); make_wff c tenv env f''], split_wf_ref f c (bind_tags (t, f) cs r env) r)
  | F.Fsum (_, t, cs, r) ->
      (C.flap (fun (_, ps) -> split_wf_params c tenv env ps) cs,
       split_wf_ref f c (bind_tags (t, f) cs r env) r)
  | F.Fabstract (_, ps, r) ->
      (split_wf_params c tenv env ps, split_wf_ref f c env r)
  | F.Farrow (l, f, f') ->
      let env' = match l with None -> env | Some p -> F.env_bind tenv env p f in
        ([make_wff c tenv env f; make_wff c tenv env' f'], [])
  | F.Fvar (_, _, r) ->
      ([], split_wf_ref f c env r)
  | F.Frec _ ->
      ([], [])
  | F.Funknown ->
      ([],[]) 

let split cs =
  assert (List.for_all (fun c -> None <> c.lc_id) cs);
  C.expand 
    (fun c -> match c.lc_cstr with SubFrame _ -> split_sub c | WFFrame _ -> split_wf c)
    cs [] 

(**************************************************************)
(********************* Constraint Indexing ********************) 
(**************************************************************)

module WH = 
  Heap.Functional(
    struct 
      type t = subref_id * (int * bool * fc_id) 
      let compare (_,(i,j,k)) (_,(i',j',k')) = 
        if i <> i' then compare i i' else 
          if j <> j' then compare j j' else compare k' k
    end)

type ref_index = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;           (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  (* C.do_catch "get_rank" (SIM.find (get_ref_id c)) sri.rank *)
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,r,_,_) ->
  Le.fold (fun _ f l -> F.qvars f @ l) env (F.refinement_qvars r)

let rhs_k = function
  | SubRef (_,_,_,(_, F.Qvar k),_) -> Some k
  | _ -> None

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm VM.empty in
  let (dm,deps) = 
    SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, List.rev_append deps deps'))
      cm (SIM.empty,[]) in
  let flabel i = C.io_to_string ((SIM.find i om).lc_id) in
  let rm = 
    List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = (SIM.find id om).lc_id in
        SIM.add id (r,b,fci) rm)
      SIM.empty (C.scc_rank flabel deps) in
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
        let o = match o with Cstr fc -> fc | _ -> assert false in
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = make_rank_map om cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig
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

(* API *)
let push_worklist sri w cs =
  List.fold_left 
    (fun w c -> 
      let id = get_ref_id c in
      let _ = C.cprintf C.ol_solve "@[Pushing@ %d@\n@]" id in 
      if Hashtbl.mem sri.pend id then w else 
        let _ = Hashtbl.replace sri.pend id () in
        WH.add (id,get_ref_rank sri c) w)
    w cs

(* API *)
let pop_worklist sri w =
  try 
    let id = fst (WH.maximum w) in
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
(*let pred_to_string p = Format.sprintf "@[%a@]" P.pprint p *)

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

let refine_simple s k1 k2 =
  let q1s  = Sol.find s k1 in
  let q2s  = Sol.find s k2 in
  let q2s' = List.filter (fun q -> List.mem q q1s) q2s in
  let _    = Sol.replace s k2 q2s' in
  let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length q2s) (List.length q2s') in
  List.length q2s' <> List.length q2s

let implies_match env sm r1 =
  let lhsm =
    Bstats.time "close_over_env" 
      (fun () ->
        List.fold_left (fun pm p -> PM.add p true pm) PM.empty 
        ((close_over_env env sm) (F.refinement_conjuncts sm qual_test_expr r1))) () in
  fun (_,p) -> 
    let rv = (not !Cf.no_simple_subs) && PM.mem p lhsm in
    let _ = if rv then incr stat_matches in rv

let implies_tp env g sm r1 = 
  let lhs = 
    let gp = Bstats.time "make guardp" (guard_predicate ()) g in
    let envp = Bstats.time "make envp" (environment_predicate sm) env in
    let r1p = Bstats.time "make r1p" (F.refinement_predicate sm qual_test_expr) r1 in
    P.big_and [envp;gp;r1p] in
  let ch = Bstats.time "TP implies" TP.implies lhs in
  fun (_,p) -> Bstats.time "ch" ch p 

let qual_wf sm env subs q =
  refinement_well_formed env sm (F.mk_refinement subs [q] []) qual_test_expr

let refine sri s c =
  let _ = incr stat_refines in
  let sm = solution_map s in 
  match c with
  | SubRef (_, _, [([], ([], [k1]))], ([], F.Qvar k2), _)
    when not (!Cf.no_simple || !Cf.verify_simple) ->
      let _ = incr stat_simple_refines in
      Bstats.time "refine_simple" (refine_simple s k1) k2
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      let _ = incr stat_sub_refines in
      let qp2s = 
        List.map 
          (fun q -> (q,F.refinement_predicate sm qual_test_expr (F.mk_refinement sub2s [q] [])))
          (sm k2) in
      let (qp2s1,qp2s') = Bstats.time "match check" (List.partition (implies_match env sm r1)) qp2s in
      let tpc = Bstats.time "implies_tp" (implies_tp env g sm) r1 in
      let (qp2s2,_)    = Bstats.time "imp check" (List.partition tpc) qp2s' in
      let _ = Bstats.time "finish" TP.finish () in
      let q2s'' = List.map fst (qp2s1 @ qp2s2) in
      let _ = Sol.replace s k2 q2s'' in
      let _ = C.cprintf C.ol_refine "@[%d --> %d@.@]" (List.length qp2s) (List.length q2s'') in
      let _ = stat_imp_queries := !stat_imp_queries + (List.length qp2s) in
      let _ = stat_valid_imp_queries := !stat_valid_imp_queries + (List.length q2s'') in
      (List.length qp2s  <> List.length q2s'')
  | WFRef (env, (subs, F.Qvar k), _) ->
      let _ = incr stat_wf_refines in
      let qs  = solution_map s k in
      let qs' = List.filter (qual_wf sm env subs) qs in
      let _   = Sol.replace s k qs' in
      (List.length qs <> List.length qs')
  | _ -> false


(**************************************************************)
(********************** Constraint Satisfaction ***************)
(**************************************************************)

let sat s = function
  | SubRef (env, g, r1, sr2, _) ->
      let gp = Bstats.time "make guardp" (guard_predicate ()) g in
      let envp = environment_predicate (solution_map s) env in
      let p1 = F.refinement_predicate (solution_map s) qual_test_expr r1 in
      let p2 = F.refinement_predicate (solution_map s) qual_test_expr (F.ref_of_simple sr2) in
      let rv = TP.implies (P.big_and [envp; gp; p1]) p2 in TP.finish ();rv
  | WFRef (env,(subs, F.Qvar k), _) as c ->
      let rv = refinement_well_formed env (solution_map s) (F.mk_refinement subs [] [k]) qual_test_expr in
      C.asserts (Printf.sprintf "ERROR: wf is unsat! (%d)" (get_ref_id c)) rv;
      rv 
  | _ -> true

let unsat_constraints sri s =
  C.map_partial
    (fun c -> if sat s c then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(**************************************************************)
(******************** Qualifier Instantiation *****************)
(**************************************************************)

module QSet = Set.Make(Qualifier)

module CLe = 
struct
    type t = F.t Le.t
    let compare = Le.setcompare
end

module CMap = Map.Make(CLe)

module VarMap = Map.Make(String)

let add_path m path = match Path.ident_name path with
  | Some name ->
      let rest = try VarMap.find name m with Not_found -> [] in VarMap.add name (path :: rest) m
  | None -> m

let make_subs m =
  VarMap.fold (fun n ps subs -> C.flap (fun p -> List.map (fun s -> (n, p) :: s) subs) ps) m [[]]

let instantiate_in_env d (qsetl, qseta) q =
  let vm   = List.fold_left add_path VarMap.empty d in
  let subs = make_subs vm in
    List.fold_left (fun (ql, qa) sub -> match Qualifier.instantiate sub q with Some q -> (QSet.add q ql, QSet.add q qa) | None -> (ql, qa)) (qsetl, qseta) subs

let instantiate_quals_in_env qs (m, qsets, qsetall) env =
  try let q = (CMap.find env m) in (m, (QSet.elements q) :: qsets, QSet.union q qsetall) with Not_found ->
    let (q, qsetall) = (List.fold_left (instantiate_in_env (Le.domain env)) (QSet.empty, qsetall) qs) in
      (CMap.add env q m, (QSet.elements q) :: qsets, qsetall)

let constraint_env (_, c) =
  (match c with | SubRef (_, _, _, _, _) -> Le.empty | WFRef (e, _, _) -> e)

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from all environments. *)
let instantiate_per_environment cs qs =
  let (_, qsets, qs) = (List.fold_left (instantiate_quals_in_env qs) (CMap.empty, [], QSet.empty) (List.map constraint_env cs)) in
  (qsets, QSet.elements qs)

(**************************************************************)
(************************ Initial Solution ********************)
(**************************************************************)

(* If a variable only ever appears on the left hand side, the variable is
 * unconstrained; this generally indicates an uncalled function.
 * When we have such an unconstrained variable, we simply say we don't
 * know anything about its value.  For uncalled functions, this will give
 * us the type which makes the least assumptions about the input. *)

let filter_wfs cs = List.filter (fun (r, _) -> match r with WFRef(_, _, _) -> true | _ -> false) cs
let filter_subs cs = List.filter (fun (r, _) -> match r with SubRef(_, _, _, _, _) -> true | _ -> false) cs
let strip_origins cs = snd (List.split cs)
                      
type solmode = WFS | LHS | RHS

let make_initial_solution cs qs =
  let s = Sol.create 1000 in
  let addrv qs = function
    | (F.Qconst _, _) -> ()
    | (F.Qvar k, LHS) -> if not (Sol.mem s k) then Sol.replace s k []
    | (F.Qvar k, RHS) -> Sol.replace s k qs
    | (F.Qvar k, _) -> if Sol.find s k != [] then Sol.replace s k qs in
  let ga (c, q) = match c with
    | SubRef (_, _, r1, (_, qe2), _) ->
        List.iter (fun qv -> addrv [] (F.Qvar qv, LHS)) (F.refinement_qvars r1); addrv qs (qe2, RHS)
    | WFRef (_, (_, qe), _) -> addrv [] (qe, LHS); addrv qs (qe, WFS) in
  let wfs = filter_wfs cs in
  let subs = filter_subs cs in
    List.iter ga subs; List.iter ga wfs; s

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)
 
let dump_constraints sri = 
  if !Cf.dump_constraints then
  (printf "@[Refinement Constraints@.@\n@]";
  iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c))

let dump_solution_stats s = 
  let kn  = Sol.length s in
  let (sum, max, min) =   
    (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
     Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
     Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
  C.cprintf C.ol_solve_stats "@[Quals:@\n\tTotal:@ %d@\n\tAvg:@ %f@\n\tMax:@ %d@\n\tMin:@ %d@\n@\n@]"
  sum ((float_of_int sum) /. (float_of_int kn)) max min;
  print_flush ()
  
let dump_solving qs sri s step =
  if step = 0 then 
    let cs = get_ref_constraints sri in 
    let qn  = List.length qs in
    let kn  = Sol.length s in
    let wcn = List.length (List.filter is_wfref_constraint cs) in
    let rcn = List.length (List.filter is_subref_constraint cs) in
    let scn = List.length (List.filter is_simple_constraint cs) in
    (dump_constraints sri;
     C.cprintf C.ol_solve_stats "@[%d@ instantiated@ qualifiers@\n@\n@]" qn; 
     C.cprintf C.ol_solve_stats "@[%d@ variables@\n@\n@]" kn;
     C.cprintf C.ol_solve_stats "@[%d@ total@ quals@\n@\n@]" (kn * qn); 
     C.cprintf C.ol_solve_stats "@[%d@ split@ wf@ constraints@\n@\n@]" wcn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ subtyping@ constraints@\n@\n@]" rcn;
     C.cprintf C.ol_solve_stats "@[%d@ simple@ subtyping@ constraints@\n@\n@]" scn;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (C.cprintf C.ol_solve_stats "@[Refine Iterations: %d@ total (wf=%d,si=%d,su=%d)\n@\n@]" 
       !stat_refines !stat_wf_refines !stat_simple_refines !stat_sub_refines;
     C.cprintf C.ol_solve_stats "@[Implication Queries: %d@ total;@ %d@ valid;@ %d@ match@]@.@." 
       !stat_imp_queries !stat_valid_imp_queries !stat_matches;
     TP.print_stats ();
     dump_solution_stats s;
     flush stdout)

let dump_solution s =
  if C.ck_olev C.ol_solve then
    Sol.iter (fun p r -> C.cprintf C.ol_solve "@[%s: %a@]@."
              (Path.unique_name p) (Oprint.print_list Qualifier.pprint C.space) r) s
  else ()

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

let rec solve_sub sri s w = 
  let _ = if !stat_refines mod 100 = 0 then C.cprintf C.ol_solve "@[num@ refines@ =@ %d@\n@]" !stat_refines in
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let (r,b,fci) = get_ref_rank sri c in
    let _ = C.cprintf C.ol_solve "@[Refining:@ %d@ in@ scc@ (%d,%b,%s):@]"
            (get_ref_id c) r b (C.io_to_string fci) in
    let w' = if Bstats.time "refine" (refine sri s) c then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub sri s w'

let solve_wf sri s = 
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine sri s c) | _ -> ())

let solve qs cs = 
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let cs = split cs in
  let (qs, allqs) = Bstats.time "instantiating quals" (instantiate_per_environment cs) qs in
  let sri = make_ref_index cs in
  let s = make_initial_solution (List.combine (strip_origins cs) qs) allqs in
  let _ = dump_solution s in
  let _ = dump_solving qs sri s 0  in 
  let _ = Bstats.time "solving wfs" (solve_wf sri) s in
  let _ = printf "@[AFTER@ WF@]@." in
  let _ = dump_solving qs sri s 1 in
  let _ = dump_solution s in
  let w = make_initial_worklist sri in
  let _ = Bstats.time "solving sub" (solve_sub sri s) w in
  let _ = dump_solving qs sri s 2 in
  let _ = dump_solution s in
  let _ = TP.reset () in
  let unsat = Bstats.time "testing solution" (unsat_constraints sri) s in
  (if List.length unsat > 0 then 
    C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
    List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat);
  (solution_map s, (List.map (fun (a, b) -> b)  unsat))
