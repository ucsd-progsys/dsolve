open Format
open Types
open Typedtree
open Ctype

module P = Predicate
module C = Common
module F = Frame
module Le = Lightenv

type mdef = (string * P.pexpr) 
type m = (constructor_tag * Path.t option list * mdef) list 
type t = m Le.t
type pre_measure = (string * (Path.t option list * mdef))

type meas_entry =
    Mcstr of pre_measure
  | Mname of string * string

let filter_cstrs es =
  C.maybe_list (List.map (function Mcstr p -> Some p | _ -> None) es)

let filter_names es =
  C.maybe_list (List.map (function Mname (n, mn) -> Some (n, mn) | _ -> None ) es)

let (empty: t) = Le.empty

let find_c p t e =
  List.filter (fun (t', _, _) -> t' = t) (Le.find p e)

let tsc (a, b, c) = (a, (b, c))

let add (p, ((tag, args, (nm, _)) as r)) menv =
  let cs = try Le.find p menv with Not_found -> [] in
  let _  = if List.exists (fun (x, y, z) -> let (nm', _) = z in tag = x && nm = nm') cs then
      failwith (Printf.sprintf "measure redef %s for path %s" nm (Path.name p)) in
  Lightenv.add p (r :: cs) menv

let sum_path = function
  | F.Fsum (p, _, _, _) -> p
  | _                   -> assert false

let rewrite_pred_funs subf r = C.app_snd (P.pexp_map_funs subf) r

let rewrite_pred_vars subf r = C.app_snd (P.pexp_map_vars (fun p -> P.Var (subf p))) r

let rewrite_pred subvars subfuns r = rewrite_pred_funs subfuns (rewrite_pred_vars subvars r)

let transl_desc mlenv (c, (ps, r)) =
  try
    let _  = if not(C.is_unique (C.maybe_list ps)) then failwith "Measure args not unique" in
    let c  = Env.lookup_constructor (Longident.parse c) mlenv in
    let _  = if List.length ps != c.cstr_arity then failwith "Wrong number of measure args" in
    let fr = F.fresh_without_vars mlenv c.cstr_res in
      Some (sum_path fr, (c.cstr_tag, ps, r))
  with Not_found ->
    None

let mk_uninterpreted env name mlname =
  try
    F.fresh_uninterpreted env ((snd (Env.lookup_value (Longident.parse mlname) env)).val_type) name
  with Not_found ->
    failwith ("Could not make uninterpreted version of undefined function: " ^ mlname)

let bms = ref empty
let mk_measures env ms = 
  let maybe_add e h =
    match transl_desc env h with
        Some k -> add k e
      | None -> e in
  bms := List.fold_left maybe_add empty ms

let mk_pred v mps (_, ps, ms) =
  let _ = if List.length ps != List.length mps then failwith "argument arity mismatch" in
  let ps = List.combine ps mps in
  let var_map v = (try match (List.assoc (Some v) ps) with Some s -> s | _ -> raise (Failure "") with Not_found -> P.Var v) in
  let (s, e) = ms in
  try 
    let e = P.pexp_map_vars var_map e in
      P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) 
  with Failure _ -> P.True

let mk_pred_list v mps mcstrs =
  List.map (mk_pred v mps) mcstrs 

let mk_preds v mps mcstrs =
  P.big_and (mk_pred_list v mps mcstrs)

let mk_pred_def v (path, tag) ps =
  mk_preds v (List.map (fun x -> Some x) ps) (find_c path tag !bms)

let mk_qual ps c =
  let v = Path.mk_ident "v" in
  (Path.mk_ident "measure", v, mk_pred_def v c ps)

let mk_single_gd menv (vp, p, tag, ps) =
  try
    Some (mk_preds vp (List.map (function Some p -> Some (P.Var p) | _ -> None) ps) (find_c p tag menv))
  with Not_found ->
    None

let mk_guard env vp cpats =
  let preds = C.map_partial (mk_single_gd !bms) cpats in
  let preds = C.flap P.conjuncts preds in
    P.big_and (List.filter (Wellformed.pred_well_formed env) preds)

(* assumes no subs *)
let rewrite_refexpr f (a, (qs, b)) = (a, (List.map (Qualifier.map_pred f) qs, b))

let map_pred_funs f (v, p) =
  (v, P.map_funs f p)

let map_exp_funs f (v, p) =
  (v, P.pexp_map_funs f p)

let transl_pred names =
  map_pred_funs (fun x -> C.sub_from_list names x)     

let transl_frame names f =
  F.map_refexprs (rewrite_refexpr (transl_pred names)) f

let pprint_menv ppf menv =
  List.iter (function | Mname(a,b) -> fprintf ppf "@[Name:@ (%s,@ %s)@]@." a b
                      | Mcstr(c, (ps, px)) -> fprintf ppf "@[Cstr:@ (%s,@ (.,@ %a))@]@." c P.pprint_pexpr (snd px)) menv

let proc_premeas env menv fenv ifenv quals =
  let (mnames, mcstrs) = (filter_names menv, filter_cstrs menv) in
  let subs = C.list_assoc_flip mnames in
  let quals = Qualmod.map_preds (transl_pred subs) quals in
  let fenv = Le.map (transl_frame subs) fenv in
  let ifenv = Le.map (transl_frame subs) ifenv in
  let mcstrs = List.map (fun (c, (ps, cstr)) -> (c, (ps, map_exp_funs (C.sub_from_list subs) cstr))) mcstrs in
  let _ = C.cprintf C.ol_dump_meas "Measures:@.%a" pprint_menv ((List.map (fun (a, (b, c)) -> Mcstr(a, (b, c))) mcstrs)) in
  let _ = mk_measures env mcstrs in
    (fenv, ifenv, quals)

let assert_constructed_expr env preds (path, tag) shp =
  let (n, v, pred) = (mk_qual preds (path, tag)) in
  let env = Le.add v shp env in
  let preds = List.filter (Wellformed.pred_well_formed env) (P.conjuncts pred) in
    (n, v, P.big_and preds)
