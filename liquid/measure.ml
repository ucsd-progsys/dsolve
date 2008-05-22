open Types
open Typedtree

module P = Predicate
module C = Common
module F = Frame
module Le = Lightenv

type mdef = (string * P.pexpr) 
type m = (constructor_tag * Path.t option list * mdef list) list 
type t = m Le.t

let (a, b, c, d) = (Path.mk_ident "a", Path.mk_ident "b", Path.mk_ident "c", Path.mk_ident "d")

let builtins = [
  ("Some", ([Some a], [("h", P.Var(a))])); 
  ("None", ([], [("h", P.PInt(0))]));
]

let builtin_funs = [
  "h";
]

let (empty: t) = Le.empty

let find_c p t e =
  List.find (fun (t', _, _) -> t' = t) (Le.find p e)

let tsc (a, b, c) = (a, (b, c))

let add (p, ((tag, _, _) as r)) env = 
  let cs = try Le.find p env with
             Not_found -> [] in
  let cs' = List.map tsc cs in
  let _ = try ignore (List.assoc tag cs'); failwith "constructor redef in measure" with 
            Not_found -> () in
  Lightenv.add p (r :: cs) env

let sum_rows_path = function
    F.Fsum(p, rows, _, _) -> (p, rows)
  | _ -> failwith "constructor result is not a constructed type?"

let sum_path = C.compose fst sum_rows_path

let transl_desc mlenv (c, (ps, rs)) =
  let _ = if not(C.is_unique ps) then failwith "Builtin measure labels not unique" in 
  let c =  
    (Env.lookup_constructor (Longident.parse c) mlenv) in
  let tag = c.cstr_tag in
  let _ = if List.length ps != c.cstr_arity then failwith "Wrong number of builtin measure labels" in
  let fr = F.fresh_without_vars mlenv c.cstr_res in
 (sum_path fr, (tag, ps, rs)) 

(* don't panic! bms only set here *)
let bms = ref empty
let mk_bms env = 
  let f g e h = add (g h) e in
  bms := List.fold_left (f (transl_desc env)) empty builtins 

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
  try 
    let (p, v) = Env.lookup_value (Longident.parse s) env in
    let fr = F.fresh_without_vars env v.val_type in
      (p, fr)
  with Not_found -> assert false

let mk_tys env =
  let ty s =
    let (p, sf) = find_mlenv_by_name s env in
    (p, mk_fun s sf) in
  List.map ty builtin_funs

let mk_pred v (_, ps, ms) mps =
  let _ = if List.length ps != List.length mps then failwith "argument arity mismatch" in
  let ps = List.combine ps mps in
  let var_map v = try P.Var (List.assoc (Some v) ps) with Not_found -> assert false in
  let cm (s, e) = 
    let e = P.pexp_map_vars var_map e in
    P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) in 
  P.big_and (List.map cm ms) 

let mk_qual ps c =
  let v = Path.mk_ident "v" in
  (Path.mk_ident "measure", v, mk_pred v c ps)

let mk_single_gd menv p vp (tag, ps) =
  try Some (mk_pred vp (find_c p tag menv) ps) with 
    Not_found -> None

let get_or_fail = function
    Texp_ident (p, _) -> p
  | _ -> failwith "only matching on idents supported; try normalizing"
let get_or_fail_pat pat = match pat.pat_desc with
    Tpat_var x -> C.i2p x
  | _ -> failwith "only matching to vars supported; try normalizing"
 
let mk_guards f e pats =
 let vp = get_or_fail e in
  let gps pat = match pat.pat_desc with
      Tpat_construct(cdesc, pl) -> (cdesc.cstr_tag, List.map get_or_fail_pat pl)
    | _ -> failwith "non-constructor matches not handled" in
  let ps = List.map gps pats in
  let p = sum_path f in
    List.map (mk_single_gd !bms p vp) ps
