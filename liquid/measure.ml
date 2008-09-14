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
  let maybe_fail = function Some p -> p | None -> raise Not_found in
  let _ = if List.length ps != List.length mps then failwith "argument arity mismatch" in
  let ps = List.combine ps mps in
  let var_map v = maybe_fail (List.assoc (Some v) ps) in
    try
      let (s, e) = ms in
      let e = P.pexp_map_vars var_map e in
      P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) 
    with Not_found -> P.True
    
let mk_preds v mps mcstrs =
  P.big_and (List.map (mk_pred v mps) mcstrs)

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

let get_patvar p = match p.pat_desc with
  | Tpat_var p | Tpat_alias (_, p) -> Some (C.i2p p)
  | _                              -> None

let cstr_res_path {cstr_res = t} = match (repr t).desc with
  | Tconstr (p, _, _) -> p
  | _                 -> assert false

let named_patterns pats =
  List.map
    (fun p -> match p.pat_desc with Tpat_alias (apat, av) -> (Some (Path.Pident av), apat) | _ -> (None, p))
    pats

let named_constructor_patterns cdesc pats = function
  | Some v -> [(v, cstr_res_path cdesc, cdesc.cstr_tag, List.map get_patvar pats)]
  | None   -> []

let constructor_patterns_aux (vo, pat) = match pat.pat_desc with
    Tpat_construct(cdesc, pl) ->
      (named_patterns pl, named_constructor_patterns cdesc pl vo)
  | Tpat_alias ({pat_desc = Tpat_construct(cdesc, pl)}, _) ->
      (named_patterns [pat], named_constructor_patterns cdesc pl vo)
  | Tpat_alias _ ->
      (named_patterns [pat], [])
  | Tpat_tuple (pl) ->
      (named_patterns pl, [])
  | _ ->
      ([], [])

let constructor_patterns expvar pat =
  C.expand constructor_patterns_aux [(expvar, pat)] []

let mk_guard vp pat =
  P.big_and (C.map_partial (mk_single_gd !bms) (constructor_patterns (Some vp) pat))

let map_pred_funs f (v, p) =
  (v, P.map_funs f p)

let transl_pred names =
  map_pred_funs (fun x -> C.rw_suff (C.sub_from_list names) x '.')     
