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


let idf = (fun s -> s)
let lu f f' s = try f s with Not_found -> f' s

let lookup3_f f1 f2 f3 fail s = lu f1 (lu f2 (lu f3 fail)) s 

let lookup2 f1 f2 s = lu f1 (lu f2 idf) s

let lookup f y s = lu f (fun s -> y) s 

let maybe_add_pref dopt s = match dopt with None -> s | Some d -> C.append_pref d s

let load_val dopt env fenv (s, pf) =
  try
    let p = C.lookup_path (match dopt with Some d -> C.append_pref d s | None -> s) env in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
      Lightenv.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let map_constructor_args dopt env (name, mlname) (cname, args, cpred) =
  let cname = lookup (fun s -> let dc = maybe_add_pref dopt s in ignore (Env.lookup_constructor (Longident.parse dc) env); dc) cname cname in
  let dargs = C.maybe_list args in
  let argmap = List.combine dargs (List.map (fun s -> Path.mk_ident s) dargs) in
  let fvar s = 
    let l_env s = C.lookup_path s env in
      lookup3_f (fun s -> List.assoc s argmap) (C.compose l_env (maybe_add_pref dopt)) l_env Path.mk_ident (C.l_to_s s) in
  let ffun s = lookup (fun s -> let s = maybe_add_pref dopt s in ignore (C.lookup_path s env); s) s s in
  let pred = P.pexp_map_funs ffun (Qualdecl.transl_patpredexp_single_map fvar cpred) in
  let args = List.map (function Some s -> Some (List.assoc s argmap) | None -> None) args in
    Mcstr(cname, (args, (maybe_add_pref dopt name, pred)))

let load_measure dopt env ((n, mn), cstrs) =
  Mname(maybe_add_pref dopt n, maybe_add_pref dopt mn)
  :: List.map (map_constructor_args dopt env (n, mn)) cstrs

let load_unint name ty env fenv ifenv menv =
  let id   = Ident.create name in
  let ty   = Typetexp.transl_type_scheme env ty in
  let env  = Env.add_value id {val_type = ty; val_kind = Val_reg} env in
  let fenv = Le.add (Path.Pident id) (F.fresh_uninterpreted env ty name) fenv in
    (env, fenv, ifenv, menv)

let is_unint_decl = function
  | LunintDecl _ -> true
  | _            -> false

let axiom_prefix = "_axiom_"

let load_axiom dopt env fenv ifenv menv name pred =
  let pred = Qualdecl.transl_patpred_single false (Path.mk_ident "") env pred in
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
            (TheoremProver.push_axiom (extract_pred f); env) else
              Le.add p f env) fenv Le.empty

let load_rw dopt rw env menv' fenv (preds, decls) =
  let load_decl (env, fenv, ifenv, menv) = function
      LvalDecl(s, f)  -> (env, fenv, load_val dopt env ifenv (s, F.translate_pframe dopt env preds f), menv)
    | LmeasDecl (name, cstrs) -> (env, fenv, ifenv, List.rev_append (load_measure dopt env (name, cstrs)) menv)
    | LunintDecl (name, ty) -> load_unint name ty env fenv ifenv menv
    | LaxiomDecl (name, pred) -> load_axiom dopt env fenv ifenv menv name pred
    | LrecrefDecl -> (env, fenv, ifenv, menv) in
  let (env, fenv, ifenv, menv) = List.fold_left load_decl (env, fenv, Lightenv.empty, []) decls in
  let (fenv, ifenv) = rw env fenv ifenv in
  let (measnames, mlnames) = List.split (M.filter_names menv) in
  let fs = List.combine (List.map Path.mk_ident measnames) (List.map2 (M.mk_uninterpreted env) measnames mlnames) in
  let fenv = Lightenv.addn fs fenv in
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
    let lfun s = lookup (fun s -> Path.name (env_lookup s)) s s in
    let lvar s = lookup env_lookup s (Path.name s) in
    let fenv = Le.fold (fun p fr e -> Le.add p (F.map_refexprs (M.rewrite_refexpr (C.app_snd (sub_pred lvar lfun))) 
                       (F.label_like fr fr)) e) ifenv fenv in
      (fenv, Le.empty) in
  List.fold_left (fun (env, menv, fenv, _) (mlq, dname) -> load_rw (Some dname) (rw dname) env menv fenv mlq) (env, [], fenv, Le.empty) mlqs

(* builtins *)

let filter_vals xs =
  C.maybe_list (List.map (function LvalDecl(x, y) -> Some(x, y)  | _ -> None) xs)
