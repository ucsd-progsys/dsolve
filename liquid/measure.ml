open Types
open Typedtree

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

let add (p, ((tag, args, ref) as r)) menv = 
  let cs = try Le.find p menv with
             Not_found -> [] in
  let (nm, _) = ref in
  let _ = if List.exists (fun (x, y, z) -> let (nm', _) = z in tag = x && nm = nm') cs then 
      let errmsg = Printf.sprintf "measure redef %s for path %s" nm (Path.name p) in
        failwith errmsg in
  Lightenv.add p (r :: cs) menv

let sum_rows_path = function
    F.Fsum(p, rows, _, _) -> (p, rows)
  | _ -> failwith "constructor result is not a constructed type?"

let sum_path = C.compose fst sum_rows_path

let rewrite_pred subs r =
    let (n, p) = r in
      (n, P.pexp_map_funs subs p)

let transl_desc mlenv subs (c, (ps, r)) =
  try
    let _ = if not(C.is_unique (C.maybe_list ps)) then failwith "Measure args not unique" in 
    let c = (Env.lookup_constructor (Longident.parse c) mlenv) in
    let tag = c.cstr_tag in
    let _ = if List.length ps != c.cstr_arity then failwith "Wrong number of measure args" in
    let fr = F.fresh_without_vars mlenv c.cstr_res in
    Some (sum_path fr, (tag, ps, rewrite_pred (fun x -> try List.assoc x subs with Not_found -> x) r)) 
  with Not_found -> None

let bms = ref empty
let mk_measures env subs ms = 
  let f g e h = 
    match g h with
        Some k -> add k e 
      | None -> e in
  bms := List.fold_left (f (transl_desc env subs)) empty ms 

let mk_fun n f = 
  let funr a = 
    let v = Path.mk_ident "v" in 
    let a = Path.Pident a in
    ([], ([(Path.mk_ident (String.concat "_" ["measure"; n]), v, P.Atom (P.Var v, P.Eq, P.FunApp(n, [P.Var a])))], [])) in  
  match f with
    F.Farrow (Some (Tpat_var a), b, f2) -> F.Farrow (Some (Tpat_var a), b, F.apply_refinement [funr a] f2)
  | F.Farrow (None, b, f2) -> 
      let a = Ident.create "x" in
        F.Farrow (Some (Tpat_var a), b, F.apply_refinement [funr a] f2)
  | _ -> failwith "not a fun in mk_fun"

let find_mlenv_by_name s env =
  let (p, v) = Env.lookup_value (Longident.parse s) env in
  let fr = F.fresh_without_vars env v.val_type in
    (p, fr)

let mk_tys env ms =
  let ty f (s, mls) =
    try
      let (p, sf) = find_mlenv_by_name mls env in
      Some (Path.mk_ident s, mk_fun s sf) 
        with Not_found -> f s in
  C.maybe_list (List.map (ty (fun x -> failwith x)) ms)

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

let mk_single_gd menv p vp tp =
      match tp with 
        | Some (tag, ps) -> 
            (try Some (mk_preds vp (List.map (function Some p -> Some (P.Var p) | _ -> None) ps) (find_c p tag menv)) with 
                Not_found -> None)
        | None -> None

let get_or_fail = function
    P.Var p -> p
  | _ -> failwith "only matching on idents supported; try normalizing"

let get_patvar p = 
  match p.pat_desc with
    Tpat_var (p) -> Some (C.i2p p)
  | Tpat_any -> None
  | _ -> raise Not_found

let mk_guard f e pat =
 let vp = get_or_fail e in
  let rec gps pat = match pat.pat_desc with
      Tpat_construct(cdesc, pl) -> 
        (try Some (cdesc.cstr_tag, (List.map get_patvar pl)) with Not_found -> None)
    | Tpat_alias (p, _) ->
        gps p
    | _ -> None in
  let ps = gps pat in
  let p = sum_path f in
    mk_single_gd !bms p vp ps

let transl_pred names (v, p) =
  (v, P.map_funs (fun x -> try List.assoc x names with Not_found -> x) p) 
