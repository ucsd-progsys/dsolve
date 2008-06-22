open Measure
open Parsetree

module F = Frame
module M = Measure
module P = Predicate

(* MLQs *)

let parse ppf env fname =
  if Sys.file_exists fname then Pparse.file ppf fname Parse.liquid_interface Config.ast_impl_magic_number else ([], [])

let load_val env fenv (s, pf) =
  try
    let p = C.lookup_path s env in
    let shape = Frame.fresh_without_vars env (C.lookup_type p env) in
    let _ = if String.contains s '.' then failwith (Printf.sprintf "mlq: val %s has invalid name" s) in
    let _ = if not(F.same_shape shape pf) then
      failwith (Printf.sprintf "mlq: val %s has shape which differs from usage" s) in
      Lightenv.add p pf fenv
  with Not_found -> failwith (Printf.sprintf "mlq: val %s does not correspond to program value" s)

let map_constructor_args env (name, mlname) (cname, args, cpred) =
  let dargs = C.maybe_list args in
  let argmap = List.combine dargs (List.map (fun s -> Path.mk_ident s) dargs) in
  let f s =
    try List.assoc s argmap with Not_found -> C.lookup_path s env in
  let pred = Qualdecl.transl_patpredexp_single_map f cpred in
  let args = List.map (function Some s -> Some (List.assoc s argmap) | None -> None) args in
    Mcstr(cname, (args, (name, pred)))

let load_measure env ((n, mn), cstrs) =
  (Mname(n, mn)) :: (List.map (map_constructor_args env (n, mn)) cstrs)  

let load env fenv (preds, decls) quals =
  let load_decl (ifenv, menv) = function
      LvalDecl(s, f)  -> (load_val env fenv (s, F.translate_pframe env preds f), menv)
    | LmeasDecl (name, cstrs) -> (ifenv, List.rev_append (load_measure env (name, cstrs)) menv)
    | LrecrefDecl -> (ifenv, menv) in
  let (ifenv, menv) = List.fold_left load_decl (Lightenv.empty, []) decls in
  let mcstrs = M.filter_cstrs menv in
  let mnames = M.filter_names menv in
  let mnsubs = C.list_assoc_flip mnames in
  let _ = M.mk_measures env mnsubs mcstrs in 
    (Lightenv.addn (M.mk_tys env mnames) fenv, ifenv, Qualmod.map_preds (M.transl_pred mnsubs) quals)

(* builtins *)

let filter_vals xs =
  C.maybe_list (List.map (function LvalDecl(x, y)  -> Some(x, y)  | _ -> None) xs)
