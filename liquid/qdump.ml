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

open Config
open Format
open Parsetree
open Asttypes

module Co = Constants
module P = Predicate
module M = Measure
module F = Frame
module Le = Liqenv
module B = Buffer
module Qg = Qualgen

let buf = B.create 80
let ppf = formatter_of_buffer buf
let vid = Path.mk_ident "_V"

let s_of_p v p = 
  let p = P.map_vars (fun v' -> if v = (Path.name v') then P.Var vid else P.Var v') p in
  B.clear buf; fprintf ppf "@[%a@]@?" P.pprint p; B.contents buf 

module QS = Set.Make(struct
                       type t = string * string 
                       let compare (_, p) (_, p') = compare p p'
                     end)

let add q qs =
  QS.add ((fun (a, b, c) -> (a, s_of_p b c)) q) qs

let patf = ref ""

let dump_qset ppf qs =
  QS.iter (fun (nm, q) -> fprintf ppf "@[qualif@ %s(%s)@ :@ %s@.@]" nm "_V" q) qs

let dump_deps ppf ds =
  List.iter (fun s -> fprintf ppf "@[mdep@ %s@]@." s) ds

let dump_intset ppf = function
  | [] -> ()
  | i :: is -> fprintf ppf "@[const_ints@ %i" i;
  List.iter (fun s -> fprintf ppf ",@ %i" s) is;
  fprintf ppf "@]@."

let dump_comment_list ppf name = function
  | [] -> ()
  | i :: is -> fprintf ppf "@[(*%s:@ %s" name i;
  List.iter (fun s -> fprintf ppf ",@ %s" s) is;
  fprintf ppf "*)@]@."

let env_bound_ids env =
  let is = ref [] in
  Le.iter 
    (fun _ f -> F.iter_labels 
      (fun p -> is := p :: !is) f)
        env; !is

let generalize_pred pred =
  let gen x =
    if Path.name x = "_V" then
      P.Var x
    else
      P.Var (Common.s_to_p ("~" ^ (String.capitalize (Path.name x)))) in
  if !Clflags.dont_gen_mlq_preds then
    pred
  else
    P.map_vars gen pred

let dump_default_qualifiers (str, env, menv, ifenv) deps qname =
  let qf = formatter_of_out_channel (open_out qname) in
  let _ = pp_set_margin qf 1230912 in
  let _ = Co.verbose_level := Co.ol_dquals in

  let prgids = Qg.bound_ids str in
  let (a, b, ids, ints) = prgids in
  let ints = Qg.CS.elements ints in
  let ids = List.fold_left (fun s i -> Qg.IS.add (Ident.name i) s) ids (env_bound_ids ifenv) in
  let ids = Qg.IS.elements ids in
  let ids = List.filter (fun s -> not (Common.tmpstring s)) ids in

  let mnms = snd (List.split (M.filter_names menv)) in
  let np n p = P.Atom(P.Var vid, P.Eq, P.FunApp(Path.mk_ident n, [P.Var (Path.mk_ident "_")])) in 
  (* TODO: instead of ids just write mvars -- change qs to patterns *)
  let mnms = Misc.tflap2 (mnms, ids) np in
  let cstrs = M.filter_cstrs menv in
  let pv vs = List.map (function Some v -> Some (P.Var v) | None -> None) vs in
  let mexprs = List.map (fun (a, (b, c)) -> (M.mk_pred vid (pv b) (a, b, c))) cstrs in
  let mexprs = List.map generalize_pred mexprs in
  let mqs = (List.fold_left (fun q e -> add ("Measure", "_V", e) q) QS.empty (mexprs @ mnms)) in
 
  let conj r l = List.rev_append (F.refinement_conjuncts (fun _ -> []) (P.Var vid) r) l in
  let fpreds = Le.flaplist (fun _ f -> F.refinement_fold conj [] f) ifenv in
  let fpreds = List.map generalize_pred (Misc.flap P.conjuncts fpreds) in
  let fqs = List.fold_left (fun q e -> add ("MLQ", "_V", e) q) QS.empty fpreds in

  let initqs = add ("FALSE", "_V", P.Atom(P.PInt(1), P.Eq, P.PInt(0))) QS.empty in
  let qs =
    if !Clflags.dont_mine_mlq_preds then
      initqs
    else
      QS.union (QS.union fqs mqs) initqs in
  dump_deps qf deps; dump_intset qf ints;
  dump_comment_list qf "Program Identifiers" ids;
  dump_qset qf qs; pp_print_flush qf ()
