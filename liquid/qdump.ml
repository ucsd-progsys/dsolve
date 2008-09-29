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

module P = Predicate
module C = Common
module M = Measure
module F = Frame
module Le = Lightenv

(*let pp v p = sprintf "%s::%a" (Path.name v) P.pprint p*)

module QS = Set.Make(struct
                       type t = string * string * P.t
                       let compare (_, _, p) (_, _, p') = compare p p'
                     end)


let patf = ref ""
            
let expand_quals env qstrs prgids =
  let expand_squal (name, pat) =
    Qualdecl.transl_pattern_valu env prgids name pat
  in
  C.flap (expand_squal) qstrs 

let dump_qset ppf qs =
  QS.iter (fun (nm, v, q) -> fprintf ppf "@[squalif@ %s(%s)@ :@ %a@.@]" nm v P.pprint q) qs

let dump_deps ppf ds =
  List.iter (fun s -> fprintf ppf "@[mdep@ %s@]@." s) ds

let dump_default_qualifiers (str, env, menv, ifenv) deps qname =
  let qf = formatter_of_out_channel (open_out qname) in
  let _ = pp_set_margin qf 1230912 in
  let _ = C.verbose_level := C.ol_dquals in

  let prgids = Qualgen.bound_ids str in
  let (_, _, ids, _) = prgids in
  let ids = List.rev_map Path.mk_ident (Qualgen.IS.elements ids) in

  let vid = Path.mk_ident "_V" in
  let mnms = snd (List.split (M.filter_names menv)) in
  let np m p = P.Atom(P.Var vid, P.Eq, P.FunApp(m, [P.Var p])) in 
  let mnms = C.tflap2 (mnms, ids) np in
  let cstrs = M.filter_cstrs menv in
  let pv vs = List.map (function Some v -> Some (P.Var v) | None -> None) vs in
  let mexprs = List.map (fun (a, (b, c)) -> (M.mk_pred vid (pv b) (a, b, c))) cstrs in
  let mqs = (List.fold_left (fun q e -> QS.add ("Measure", "_V", e) q) QS.empty (mexprs @ mnms)) in
 
  let conj r l = List.rev_append (F.refinement_conjuncts (fun _ -> assert false) (P.Var vid) r) l in
  let fpreds = Le.flaplist (fun _ f -> F.refinement_fold conj [] f) ifenv in
  let fpreds = C.flap P.conjuncts fpreds in
  let fqs = List.fold_left (fun q e -> QS.add ("MLQ", "_V", e) q) QS.empty fpreds in

  let dqstrs = Pparse.file std_formatter !patf Parse.qualifier_patterns ast_impl_magic_number in
  let dqstrs = expand_quals env dqstrs prgids in
  let initqs = QS.add ("FALSE", "_V", P.Atom(P.PInt(1), P.Eq, P.PInt(0))) QS.empty in
  let qs = List.fold_left (fun qs q -> QS.add q qs) initqs dqstrs in
  let qs = QS.union (QS.union qs fqs) mqs in
  dump_deps qf deps; dump_qset qf qs; pp_print_flush qf ()
