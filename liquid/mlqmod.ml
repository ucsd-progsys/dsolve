(*
 * Copyright © 2010 The Regents of the University of California. All rights reserved.
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

open Measure
open Parsetree
open Format
open Types
open Misc.Ops

module F = Frame
module M = Measure
module P = Predicate
module TP = TheoremProver
module Qd = Qualdecl
module T = Typetexp

(* MLQs *)

let parse ppf fname =
  if Sys.file_exists fname then Pparse.file ppf fname Parse.liquid_interface Config.ast_impl_magic_number else ([])

let idf = (fun s -> s)
let lu f f' s = try f s with Not_found -> f' s

let lookup3_f f1 f2 f3 fail s = lu f1 (lu f2 (lu f3 fail)) s 

let lookup2_f f1 f2 fail s = lu f1 (lu f2 fail) s

let lookup f y s = lu f (fun s -> y) s 

let maybe_add_pref dopt s = match dopt with None -> s | Some d -> C.append_pref d s

let nativize_core_type dopt env ty =
  C.map_core_type_constrs (fun l ->
     (lookup (fun s -> let s = Longident.parse (maybe_add_pref dopt (C.l_to_s l)) in 
       ignore (Env.lookup_type s env); s) l l)) ty

let translate_pframe dopt env pf =
  let vars = ref [] in
  let getvar a = try List.find (fun b -> Ident.name b = a) !vars
                   with Not_found -> let a = Ident.create a in
                   let _ = vars := a::!vars in
                     a in
  let transl_lident l = match dopt with Some d -> Longident.parse (C.append_pref d (C.l_to_s l)) | None -> l in 
  let lookup l = 
    try Env.lookup_type (transl_lident l) env 
      with Not_found -> try Env.lookup_type l env
        with Not_found -> raise (T.Error(Location.none, T.Unbound_type_constructor l)) in
  let transl_pref x = 
    let r = Qd.transl_pref (fun x -> try [C.lookup_path x env] with Not_found -> [Path.mk_ident x]) x in
    (*let _ = List.iter (fun (_, (r, _)) -> List.iter (fun r -> printf "%a@." Qualifier.pprint r) r) r in*)
    r in
  let rec transl_pframe_rec cstack pf =
    match pf with
    | PFvar (a, subs, r) -> F.Fvar (getvar a, F.generic_level, subs, transl_pref r)
    | PFsum (l, rro, ps, cs, r) -> transl_sum cstack l rro ps cs r
    | PFconstr (l, rro, fs, i, r) -> transl_constr cstack l rro fs i r
    | PFarrow (v, a, b) ->
        let x = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem id !vars then failwith "Redefined variable";
                vars := id :: !vars; id
          | None -> F.fresh_binder () in
        F.Farrow (x, transl_pframe_rec cstack a, transl_pframe_rec cstack b)
    | PFtuple (fs, r) -> 
        F.tuple_of_frames (List.map (transl_pframe_rec cstack) fs) (transl_pref r)
    | PFrecord (fs, r) -> transl_record cstack fs r
  and transl_sum cstack l prr ps cs r =
    let path, decl = lookup l in
    let rr         = transl_recref_opt decl prr in
    let ps         = List.map (transl_tyformal cstack) ps in
    let cstrs      = snd (List.split (Env.constructors_of_type path (Env.find_type path env))) in
      F.Finductive (path, ps, rr, transl_cstrs (path :: cstack) (List.combine cs cstrs), transl_pref r)
  and transl_tyformal cstack (id, pf) =
    (getvar id, transl_pframe_rec cstack pf, F.Covariant)
  and transl_cstrs cstack cs =
    List.map (fun (ps, cstr) -> (cstr.cstr_tag, ("", transl_params cstack ps))) cs
  and transl_params cstack ps =
    List.map (fun (id, f) -> (Ident.create id, transl_pframe_rec cstack f, F.Covariant)) ps
  and transl_recref rr =
    List.map (fun rs -> List.map transl_pref rs) rr
  and transl_recref_opt decl = function
    | Some rr -> transl_recref rr
    | None    ->
        match decl.type_kind with
          | Type_abstract               -> assert false
          | Type_record  (fields, _, _) -> [List.map (fun _ -> F.empty_refinement) fields]
          | Type_variant (cdecls, _)    -> List.map (snd <+> List.map (fun _ -> F.empty_refinement)) cdecls
  and transl_constr cstack l rro fs id r =
    let (ls, fs) = List.split fs in
    let id = match id with Some id -> Ident.create id | None -> if C.empty_list fs then C.dummy_id else C.abstr_elem_id () in
    let ls = Miscutil.mapi
      (fun l i -> match l with Some l -> Ident.create l | None -> C.tuple_elem_id i) ls in
    let params = List.map (transl_pframe_rec cstack) fs in
    let (path, decl) = lookup l in
      if List.exists (Path.same path) cstack then
        F.Frec (path, params, transl_recref_opt decl rro, transl_pref r)
      else
        let _ = if List.length fs != decl.type_arity then
          raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
        let varis = List.map F.translate_variance decl.type_variance in
          match decl.type_kind with 
              Type_abstract ->
                F.abstract_of_params_with_labels ls path params varis id (transl_pref r)
            | Type_record(fields, _, _) ->
                (* fresh_record (fresh_without_vars env) path fields *) assert false
            | Type_variant _ ->
                params |> F.fresh_variant_with_params env path |> F.apply_refinement (transl_pref r)
  and transl_record cstack fs r =
    let ps = List.map (fun (f, s, m) -> (Ident.create s, transl_pframe_rec cstack f, F.mutable_variance m)) fs in
    let path = Path.mk_ident "_anon_record" in
      F.sum_of_params path ps (transl_pref r) (* pmr: TODO *)
  in transl_pframe_rec [] pf

let load_val dopt env fenv (s, pf) =
  try
    let p = C.lookup_path (match dopt with Some d -> C.append_pref d s | None -> s) env in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
      Le.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let map_constructor_args dopt env (name, mlname) (cname, args, cpredorexp) =
  let cname = lookup (fun s -> let dc = maybe_add_pref dopt s in ignore (Env.lookup_constructor (Longident.parse dc) env); dc) cname cname in
  let dargs = C.maybe_list args in
  let argmap = List.combine dargs (List.map (fun s -> Path.mk_ident s) dargs) in
  let fvar s = 
    let l_env s = [C.lookup_path s env] in
    let l_assoc s = [List.assoc s argmap] in
      lookup3_f l_assoc (C.compose l_env (maybe_add_pref dopt)) l_env (fun x -> assert false) s in
  let ffun s = lookup (fun s -> let s = maybe_add_pref dopt (Path.name s) in C.lookup_path s env) s s in
  let pred =
    P.pred_or_pexp_map_funs ffun 
     (C.ex_one "metavariable or ident set used in measure"
      (Qualdecl.transl_patpredorexp_map fvar fvar cpredorexp)) in
  let args = List.map (function Some s -> Some (List.assoc s argmap) | None -> None) args in
    Mcstr(cname, (args, (maybe_add_pref dopt name, pred)))

let load_measure dopt env ((n, mn), cstrs) =
  Mname (maybe_add_pref dopt n, maybe_add_pref dopt mn)
  :: List.map (map_constructor_args dopt env (n, mn)) cstrs

let load_unint name ty env fenv ifenv menv =
  let id   = Ident.create name in
  let ty   = Typetexp.transl_type_scheme env ty in
  let name = Path.Pident id in
  let env  = Env.add_value id {val_type = ty; val_kind = Val_reg} env in
  let fenv = Le.add name (F.fresh_uninterpreted env ty name) fenv in
    (env, fenv, ifenv, menv)

let is_unint_decl = function
  | LunintDecl _ -> true
  | _            -> false

let load_embed dopt ty psort env fenv ifenv menv =
  let ty = Typetexp.transl_type_scheme env (nativize_core_type dopt env ty) in
  let _ = TP.Prover.embed_type (F.fresh_without_vars env ty, psort) in
    (env, fenv, ifenv, menv)

let axiom_prefix = "_axiom_"

let load_axiom dopt env fenv ifenv menv name pred =
  let lu =
    lookup2_f (fun x -> [C.lookup_path (maybe_add_pref dopt x) env])
              (fun x -> [Le.find_path x fenv])
              (fun x -> failwith (sprintf "Axiom@ %s@ uses@ unbound@ identifier@ %s" name x)) in
  let pred = C.ex_one "patterns used in axiom decl" (Qualdecl.transl_patpred_map lu lu pred) in
  let fr = Builtins.rUnit "" (Path.mk_ident "") pred in 
  let add = Le.add (Path.mk_ident (axiom_prefix ^ name)) fr in
  let (fenv, ifenv) = match dopt with Some _ -> (fenv, add ifenv) | None -> (add fenv, ifenv) in
    (env, fenv, ifenv, menv)

let extract_pred f =
  match F.get_refinement f with
  | Some [([], ([(_, _, p)], []))] -> p
  | _ -> assert false

let scrub_and_push_axioms fenv =
  Le.fold (fun p f env -> if C.has_prefix axiom_prefix (Path.name p) then 
            (TheoremProver.push_axiom fenv (extract_pred f); env) else
              Le.add p f env) fenv Le.empty

let scrub_axioms fenv =
  Le.fold (fun p f env -> if C.has_prefix axiom_prefix (Path.name p) then 
            env else Le.add p f env) fenv Le.empty

let load_rw dopt rw env menv' fenv decls =
  let load_decl (env, fenv, ifenv, menv) = function
      LvalDecl(s, f) -> (env, fenv, load_val dopt env ifenv (s, translate_pframe dopt env f), menv)
    | LmeasDecl (name, cstrs) -> (env, fenv, ifenv, List.rev_append (load_measure dopt env (name, cstrs)) menv)
    | LunintDecl (name, ty) -> load_unint name ty env fenv ifenv menv
    | LembedDecl (ty, psort) -> load_embed dopt ty psort env fenv ifenv menv
    | LaxiomDecl (name, pred) -> load_axiom dopt env fenv ifenv menv name pred
    | LrecrefDecl -> (env, fenv, ifenv, menv) in
  let (env, fenv, ifenv, menv) = List.fold_left load_decl (env, fenv, Le.empty, []) decls in
  let (fenv, ifenv) = rw env fenv ifenv in
  let (measnames, mlnames) = List.split (M.filter_names menv) in
  let measpaths = List.map Path.mk_ident measnames in
  let _ = M.set_paths (List.combine measnames measpaths) in 
  let fs = List.combine measpaths (List.map2 (M.mk_uninterpreted env) measpaths mlnames) in
  (*we could also add an uninterp for the mlname if not in source*)
  let fenv = Le.addn fs fenv in
    (env, List.rev_append menv menv', fenv, ifenv)

let rewrite_ref f r = List.map (M.rewrite_refexpr f) r
let rewrite_recref f rr = List.map (List.map (rewrite_ref f)) rr 

let load_local_sig env fenv mlq =
  let rw env fenv ifenv = 
    let ifenv = Le.map (fun fr -> F.label_like fr fr) ifenv in
    (fenv, ifenv) in
  load_rw None rw env [] fenv mlq

let sub_pred v f p =
  P.map_vars (fun x -> P.Var (v x)) (P.map_funs f p)

let load_dep_sigs env fenv mlqs =
  let rw dname env fenv ifenv =
    let modname s = C.append_pref dname s in
    let env_lookup s = let s = modname s in C.lookup_path s env in
    let fenv_lookup s = let s = modname s in Le.find_path s fenv in
    let lu s = lookup2_f env_lookup fenv_lookup (fun _ -> s) (Path.name s) in
    let fenv = Le.fold (fun p fr e -> Le.add p (F.map_refexprs (M.rewrite_refexpr (C.app_snd (sub_pred lu lu))) 
                       (F.label_like fr fr)) e) ifenv fenv in
      (fenv, Le.empty) in
  List.fold_left (fun (env, menv, fenv, _) (mlq, dname) -> load_rw (Some dname) (rw dname) env menv fenv mlq) (env, [], fenv, Le.empty) mlqs

(* builtins *)

let filter_vals xs =
  C.maybe_list (List.map (function LvalDecl(x, y) -> Some(x, y)  | _ -> None) xs)
