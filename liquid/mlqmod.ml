open Measure
open Parsetree
open Format
open Types

module Tt = Typedtree
module F = Frame
module M = Measure
module P = Predicate
module TP = TheoremProverZ3
module QF = Qualifymod
module Qd = Qualdecl
module Le = Le
module T = Typetexp

(* MLQs *)

let parse ppf fname =
  if Sys.file_exists fname then Pparse.file ppf fname Parse.liquid_interface Config.ast_impl_magic_number else ([])

let idf = (fun s -> s)
let lu f f' s = try f s with Not_found -> f' s

let lookup3_f f1 f2 f3 fail s = lu f1 (lu f2 (lu f3 fail)) s 

let lookup2 f1 f2 s = lu f1 (lu f2 idf) s

let lookup f y s = lu f (fun s -> y) s 

let maybe_add_pref dopt s = match dopt with None -> s | Some d -> C.append_pref d s

let nativize_core_type dopt env ty =
  C.map_core_type_constrs (fun l ->
     (lookup (fun s -> let s = Longident.parse (maybe_add_pref dopt (C.l_to_s l)) in 
       ignore (Env.lookup_type s env); s) l l)) ty

let rec translate_pframe dopt env fenv pf =
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
  let transl_pref = Qd.transl_pref (fun x -> try [C.lookup_path x env] with Not_found -> [Path.mk_ident x]) in
  let rec transl_pframe_rec pf =
    match pf with
    | PFvar (a, subs, r) -> F.Fvar (getvar a, F.generic_level, subs, transl_pref r)
    | PFrec (a, rr, r) -> F.Frec (getvar a, transl_recref rr, transl_pref r)
    | PFsum (l, rro, cs, r) -> transl_sum l rro cs r
    | PFconstr (l, fs, i, r) -> transl_constr l fs i r
    | PFarrow (v, a, b) ->
        let pat = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem (Path.Pident id) !vars then failwith "Redefined variable";
                vars := Path.Pident id :: !vars; Tt.Tpat_var id
          | None -> F.fresh_binder () in
        F.Farrow (pat, transl_pframe_rec a, transl_pframe_rec b)
    | PFtuple (fs, r) -> 
        F.tuple_of_frames (List.map transl_pframe_rec fs) (transl_pref r)
    | PFrecord (fs, r) -> transl_record fs r
  and transl_sum l rro cs r =
    let (path, decl) = lookup l in
    let cstrs = snd (List.split (Env.constructors_of_type path (Env.find_type path env))) in
    let rro = match rro with None -> None | Some (rvar, rr) -> Some (getvar rvar, transl_recref rr) in
      F.Fsum (path, rro, transl_cstrs (List.combine cs cstrs), transl_pref r)
  and transl_cstrs = function
    | []                  -> []
    | (ps, cstr) :: cstrs -> (cstr.cstr_tag, ("", transl_params ps)) :: transl_cstrs cstrs
  and transl_params = function
    | [] -> []
        (* pmr: need real variance here *)
    | (id, f) :: ps -> (Ident.create id, transl_pframe_rec f, F.Covariant) :: transl_params ps
  and transl_recref rr =
    List.map (fun rs -> List.map transl_pref rs) rr
  and transl_constr l fs id r =
    let (ls, fs) = List.split fs in
    let id = match id with Some id -> Ident.create id | None -> if C.empty_list fs then C.dummy_id else C.abstr_elem_id () in
    let ls = Misc.mapi
      (fun l i -> match l with Some l -> Ident.create l | None -> C.tuple_elem_id i) ls in
    let params = List.map transl_pframe_rec fs in
    let (path, decl) = lookup l in
    let _ = if List.length fs != decl.type_arity then
      raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
    let varis = List.map F.translate_variance decl.type_variance in
      match decl.type_kind with 
          Type_abstract ->
            F.abstract_of_params_with_labels ls path params varis id (transl_pref r)
        | Type_record(fields, _, _) ->
            (* fresh_record (fresh_without_vars env) path fields *) assert false
        | Type_variant _ ->
            match List.split (Env.constructors_of_type path (Env.find_type path env)) with
              | (_, cstr :: _) -> F.apply_refinement (transl_pref r) 
                                    (F.fresh_without_vars env (snd (Ctype.instance_constructor cstr)))
              | _              -> failwith "Annotated type has no constructors!"
  and transl_record fs r =
    let ps = List.map (fun (f, s, m) -> (Ident.create s, transl_pframe_rec f, F.mutable_variance m)) fs in
    let path = Path.mk_ident "_anon_record" in
      F.record_of_params path ps (transl_pref r)
  in transl_pframe_rec pf

let load_val dopt env fenv (s, pf) =
  try
    let p = C.lookup_path (match dopt with Some d -> C.append_pref d s | None -> s) env in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
      Le.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let load_nrval dopt env fenv (s, pf) =
  try
    let p = C.lookup_path (match dopt with Some d -> C.append_pref d s | None -> s) env in
    let pf' = F.fresh env (Env.find_value p env).val_type in
    let pf = F.label_like pf' pf in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
    QF.add_nrframe pf; Le.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let map_constructor_args dopt env (name, mlname) (cname, args, cpred) =
  let cname = lookup (fun s -> let dc = maybe_add_pref dopt s in ignore (Env.lookup_constructor (Longident.parse dc) env); dc) cname cname in
  let dargs = C.maybe_list args in
  let argmap = List.combine dargs (List.map (fun s -> Path.mk_ident s) dargs) in
  let fvar s = 
    let l_env s = [C.lookup_path s env] in
    let l_assoc s = [List.assoc s argmap] in
      lookup3_f l_assoc (C.compose l_env (maybe_add_pref dopt)) l_env (fun x -> assert false) s in
  let ffun s = lookup (fun s -> let s = maybe_add_pref dopt (Path.name s) in C.lookup_path s env) s s in
  (*let x = Qualdecl.transl_patpredexp_map fvar fvar cpred in (* DEBUG *)
  let _ = List.rev_map (fun p -> printf "%a@.@." P.pprint_pexpr p) x in (* DEBUG *)*)
  let pred = P.pexp_map_funs ffun (C.ex_one "metavariable or ident set used in measure" (Qualdecl.transl_patpredexp_map fvar fvar cpred)) in
  let args = List.map (function Some s -> Some (List.assoc s argmap) | None -> None) args in
    Mcstr(cname, (args, (maybe_add_pref dopt name, pred)))

let load_measure dopt env ((n, mn), cstrs) =
  Mname (maybe_add_pref dopt n, maybe_add_pref dopt mn)
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

let load_embed dopt ty psort env fenv ifenv menv =
  let ty = Typetexp.transl_type_scheme env (nativize_core_type dopt env ty) in
  let _ = TP.Prover.embed_type (F.fresh_without_vars env ty, psort) in
    (env, fenv, ifenv, menv)

let axiom_prefix = "_axiom_"

let load_axiom dopt env fenv ifenv menv name pred =
  let lu = lu (fun x -> [C.lookup_path (maybe_add_pref dopt x) env])
    (fun x -> failwith (sprintf "Axiom@ %s@ uses@ unbound@ identifier@ %s" name x)) in
  let pred = C.ex_one "patterns used in axiom decl" (Qualdecl.transl_patpred_map lu lu pred) in
  (*let pred = C.ex_one "patterns used in axiom decl" (Qualdecl.transl_patpred_simple (Le.combine fenv ifenv) pred) in*)
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
      LvalDecl(s, f) -> (env, fenv, load_val dopt env ifenv (s, translate_pframe dopt env fenv f), menv)
    | LnrvalDecl(s, f) -> (env, fenv, load_nrval dopt env ifenv (s, translate_pframe dopt env fenv f), menv)
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
  let fs = List.combine measpaths (List.map2 (M.mk_uninterpreted env) measnames mlnames) in
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
    let lfun s = lookup env_lookup s (Path.name s) in
    let lvar s = lookup env_lookup s (Path.name s) in
    let fenv = Le.fold (fun p fr e -> Le.add p (F.map_refexprs (M.rewrite_refexpr (C.app_snd (sub_pred lvar lfun))) 
                       (F.label_like fr fr)) e) ifenv fenv in
      (fenv, Le.empty) in
  List.fold_left (fun (env, menv, fenv, _) (mlq, dname) -> load_rw (Some dname) (rw dname) env menv fenv mlq) (env, [], fenv, Le.empty) mlqs

(* builtins *)

let filter_vals xs =
  C.maybe_list (List.map (function LvalDecl(x, y) -> Some(x, y)  | _ -> None) xs)
