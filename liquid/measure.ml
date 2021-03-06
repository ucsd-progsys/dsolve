open Format
open Types
open Typedtree
open Ctype
module Co = Constants
module P  = Predicate
module F  = Frame
module Le = Liqenv
module Qd = Qualdecl

open Misc.Ops

type mdef = (string * P.t_or_pexpr) 
type m = (constructor_tag * Path.t option list * mdef) list 
type t = m Le.t
type pre_measure = (string * (Path.t option list * mdef))

type meas_entry =
    Mcstr of pre_measure
  | Mname of string * string

let filter_cstrs es =
  Misc.maybe_list (List.map (function Mcstr p -> Some p | _ -> None) es)

let filter_names es =
  Misc.maybe_list (List.map (function Mname (n, mn) -> Some (n, mn) | _ -> None ) es)

let (empty: t) = Le.empty

let find_c p t e =
  List.filter (fun (t', _, _) -> t' = t) (Le.find p e)

let tsc (a, b, c) = (a, (b, c))

let add (p, ((tag, args, (nm, _)) as r)) menv =
  let cs = try Le.find p menv with Not_found -> [] in
  let _  = if List.exists (fun (x, y, z) -> let (nm', _) = z in tag = x && nm = nm') cs then
      failwith (Printf.sprintf "measure redef %s for path %s" nm (Path.name p)) in
  Liqenv.add p (r :: cs) menv

let sum_path = function
  | F.Fsum (p, _, _)             -> p
  | F.Finductive (p, _, _, _, _) -> p
  | _                            -> assert false

let rewrite_pred_funs subf r = Misc.app_snd (P.pexp_map_funs subf) r

let rewrite_pred_vars subf r = Misc.app_snd (P.pexp_map_vars (fun p -> P.Var (subf p))) r

let rewrite_pred subvars subfuns r = rewrite_pred_funs subfuns (rewrite_pred_vars subvars r)

let transl_desc mlenv (c, (ps, r)) =
  try
    let _  = if ps |> Misc.maybe_list |> Misc.is_unique |> not then failwith "Measure args not unique" in
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

(* pretend these refs are hidden *)
let bms = ref empty
let mk_measures env ms = 
  let maybe_add e h =
    match transl_desc env h with
        Some k -> add k e
      | None -> e in
  bms := List.fold_left maybe_add empty ms

let paths = ref []
let set_paths pns = paths := List.rev_append pns !paths
let get_path s = List.assoc s !paths

let mk_pred v mps (_, ps, ms) =
  let _ = if List.length ps != List.length mps then failwith "argument arity mismatch" in
  let ps = List.combine ps mps in
  let var_map v = (try match (List.assoc (Some v) ps) with Some s -> s | _ -> raise (Failure "") with Not_found -> P.Var v) in
  let (s, p_or_e) = ms in
  try 
    let p_or_e = P.pred_or_pexp_map_vars var_map p_or_e in
    let s_of_v = P.FunApp(get_path s, [P.Var v]) in
    match p_or_e with
    | P.Pexpr e ->
        P.Atom(s_of_v, P.Eq, e) 
    | P.Pt t ->
        match t with
        | P.True -> P.Boolexp(s_of_v)
        | P.Not(P.True) -> P.Not (P.Boolexp(s_of_v))
        | _ -> P.Iff(P.Boolexp(s_of_v), t)
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
  let preds = Misc.map_partial (mk_single_gd !bms) cpats in
  let preds = Misc.flap P.conjuncts preds in
  let preds = List.filter (Wellformed.pred_well_formed env) preds in
    P.big_and preds

(* assumes no subs *)
let rewrite_refexpr f (a, (qs, b)) = (a, (List.map (Qualifier.map_pred f) qs, b))

let map_pred_funs f (v, p) =
  (v, P.map_funs f p)

let map_exp_funs f (v, p) =
  (v, P.pexp_map_funs f p)

let map_funs f (v, p_or_e) =
  (v, P.pred_or_pexp_map_funs f p_or_e)

let transl_pred names =
  map_pred_funs (fun x -> Common.sub_from_list names x) 

let transl_frame names f =
  F.map_refexprs (rewrite_refexpr (transl_pred names)) f

let qualpat_map_predpat f q =
  {q with Parsetree.pqual_pat_desc = Misc.app_thd3 f q.Parsetree.pqual_pat_desc}

let transl_qualpat names =
  (Common.l_to_s <+> Common.sub_from_list names <+> Common.s_to_l)
  |> Qd.pat_map_funs
  |> qualpat_map_predpat 

let pprint_menv ppf menv =
  List.iter (function | Mname(a,b) -> fprintf ppf "@[Name:@ (%s,@ %s)@]@." a b
                      | Mcstr(c, (ps, px)) -> fprintf ppf "@[Cstr:@ (%s,@ (.,@ %a))@]@." c P.pprint_t_or_pexpr (snd px)) menv

let proc_premeas env menv fenv ifenv quals =
  let (mnames, mcstrs) = (filter_names menv, filter_cstrs menv) in
  let subs = Misc.list_assoc_flip mnames in
  let quals = List.rev_map (Misc.app_snd (transl_qualpat subs)) quals in
  let subpaths = List.map (fun (x, y) -> (Common.lookup_path x env, get_path y)) subs in
  let fenv = Le.map (transl_frame subpaths) fenv in
  let ifenv = Le.map (transl_frame subpaths) ifenv in
  let mcstrs = List.map (fun (c, (ps, cstr)) -> (c, (ps, map_funs (Common.sub_from_list subpaths) cstr))) mcstrs in
  let _ = Co.cprintf Co.ol_dump_meas "Measures:@.%a" pprint_menv ((List.map (fun (a, (b, c)) -> Mcstr(a, (b, c))) mcstrs)) in
  let _ = mk_measures env mcstrs in
    (fenv, ifenv, quals)

let assert_constructed_expr env preds (path, tag) shp =
  let (n, v, pred) = (mk_qual preds (path, tag)) in
  let env = Le.add v shp env in
  let preds = List.filter (Wellformed.pred_well_formed env) (P.conjuncts pred) in
    (n, v, P.big_and preds)
