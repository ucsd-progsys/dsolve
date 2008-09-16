open Measure
open Parsetree
open Format
open Types

module F = Frame
module M = Measure
module P = Predicate

(* MLQs *)

let parse ppf fname =
  if Sys.file_exists fname then Pparse.file ppf fname Parse.liquid_interface Config.ast_impl_magic_number else ([], [])

let load_val dopt env fenv (s, pf) =
  try
    let p = C.lookup_path (match dopt with Some d -> C.append_pref d s | None -> s) env in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
      Lightenv.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let map_constructor_args env (name, mlname) (cname, args, cpred) =
  let dargs = C.maybe_list args in
  let argmap = List.combine dargs (List.map (fun s -> Path.mk_ident s) dargs) in
  let f s =
    let s = String.concat "." (Longident.flatten s) in
    try List.assoc s argmap with Not_found -> C.lookup_path s env in
  let pred = Qualdecl.transl_patpredexp_single_map f cpred in
  let args = List.map (function Some s -> Some (List.assoc s argmap) | None -> None) args in
    Mcstr(cname, (args, (name, pred)))

let load_measure env ((n, mn), cstrs) =
  Mname(n, mn) :: List.map (map_constructor_args env (n, mn)) cstrs

let load_unint name ty env fenv ifenv menv =
  let id   = Ident.create name in
  let ty   = Typetexp.transl_type_scheme env ty in
  let env  = Env.add_value id {val_type = ty; val_kind = Val_reg} env in
  let fenv = Le.add (Path.Pident id) (F.fresh_uninterpreted env ty name) fenv in
    (env, fenv, ifenv, menv)

let is_unint_decl = function
  | LunintDecl _ -> true
  | _            -> false

let load_axiom dopt env fenv ifenv menv name pred =
  let pred = Qualdecl.transl_patpred_single false (Path.mk_ident "") env pred in
  let fr = Builtins.rUnit "" (Path.mk_ident "") pred in 
  let add = Le.add (Path.mk_ident ("axiom_" ^ name)) fr in
  let (fenv, ifenv) = match dopt with Some _ -> (fenv, add ifenv) | None -> (add fenv, ifenv) in
    (env, fenv, ifenv, menv)

let load_rw dopt rw env fenv (preds, decls) quals =
  let load_decl (env, fenv, ifenv, menv) = function
      LvalDecl(s, f)  -> (env, fenv, load_val dopt env ifenv (s, F.translate_pframe dopt env preds f), menv)
    | LmeasDecl (name, cstrs) -> (env, fenv, ifenv, List.rev_append (load_measure env (name, cstrs)) menv)
    | LunintDecl (name, ty) -> load_unint name ty env fenv ifenv menv
    | LaxiomDecl (name, pred) -> load_axiom dopt env fenv ifenv menv name pred
    | LrecrefDecl -> (env, fenv, ifenv, menv) in
  let (env, fenv, ifenv, menv) = List.fold_left load_decl (env, fenv, Lightenv.empty, []) decls in
  let (mcstrs, mnames, qsubs, fenv, ifenv) = rw env menv fenv ifenv in
  let (measnames, mlnames) = List.split mnames in
  let fs = List.combine (List.map Path.mk_ident measnames) (List.map2 (M.mk_uninterpreted env) measnames mlnames) in
  let fenv = Lightenv.addn fs fenv in
  let _ = M.mk_measures env mcstrs in 
  let quals = Qualmod.map_preds (M.transl_pred qsubs) quals in
    (env, fenv, ifenv, quals)

let hier_lookup f1 f2 s =
  try f1 s with Not_found -> try f2 s with Not_found -> s 

let lookup f y s =
  try f s with Not_found -> y

(* assumes no subs *)
let rewrite_refexpr f (a, (qs, b)) = (a, (List.map (Qualifier.map_pred f) qs, b))

let rewrite_ref f r = List.map (rewrite_refexpr f) r
let rewrite_recref f rr = List.map (List.map (rewrite_ref f)) rr 

let load_local_sig env fenv mlq quals =
  let f s (c, (ps, r)) = (c, (ps, M.rewrite_pred_funs (C.sub_from_list s) r)) in 
  let rw env menv fenv ifenv =
    let mnames = M.filter_names menv in
    let mnsubs = C.list_assoc_flip mnames in
    let mcstrs = List.map (f mnsubs) (M.filter_cstrs menv) in
    let ifenv = Le.map (fun f -> F.map_refexprs (rewrite_refexpr (M.transl_pred mnsubs)) f) ifenv in
      (mcstrs, mnames, mnsubs, fenv, ifenv) in
  load_rw None rw env fenv mlq quals

let sub_pred v f p =
  P.map_vars (fun x -> P.Var (v x)) (P.map_funs f p)

let load_dep_sigs env fenv mlqs quals =
  let rw dname env menv fenv ifenv =
    let modname s = C.append_pref dname s in
    let env_lookup s = let s = modname s in C.lookup_path s env in
    let mnames = M.filter_names menv in
    let simplesubs = C.list_assoc_flip mnames in
    let mnames = List.map (C.app_pr modname) mnames in
    let lfun = hier_lookup (fun s -> modname (List.assoc s simplesubs)) (C.compose Path.name env_lookup) in
    let f (c, (ps, r)) = 
      let lvar = hier_lookup (fun x -> List.find (fun s -> s = x) (C.maybe_list ps)) (C.compose env_lookup Path.name) in
        (modname c, (ps, M.rewrite_pred lvar lfun r)) in
    let mcstrs = List.map f (M.filter_cstrs menv) in
    let lvar s = lookup env_lookup s (Path.name s) in
    let fenv = Le.fold (fun p fr e -> Le.add p (F.map_refexprs (rewrite_refexpr (C.app_snd (sub_pred lvar lfun))) fr) e) ifenv fenv in
      (mcstrs, mnames, simplesubs, fenv, Le.empty) in
  List.fold_left (fun (env, fenv, _, quals) (mlq, dname) -> load_rw (Some dname) (rw dname) env fenv mlq quals) (env, fenv, Le.empty, quals) mlqs

(* builtins *)

let filter_vals xs =
  C.maybe_list (List.map (function LvalDecl(x, y) -> Some(x, y)  | _ -> None) xs)
