open Types

module P = Predicate
module C = Constraint
module F = Frame
module Le = Lightenv
module Ty = Typedtree

type mdef = (string * P.pexpr) 
type m = (constructor_tag * Path.t option list * mdef list) list 
type t = m Le.t

let builtins = [
  ("Some", ([], [("h", P.PInt(1))])); 
  ("None", ([], [("h", P.PInt(0))]));
]

let builtin_funs = [
  "h";
]

let (empty: t) = Le.empty

let find_c p t e =
  List.find (fun (t', _, _) -> t' = t) (Le.find p e)

let tsc (a, b, c) = (a, (b, c))

let find_by_name s e =
  match Le.filterlist (fun p v -> (Path.name p) = s) e with
    | x :: [] -> x
    | x :: xs -> assert false
    | [] -> raise Not_found

let add (p, ((tag, _, _) as r)) env = 
  let cs = try Le.find p env with
             Not_found -> [] in
  let cs' = List.map tsc cs in
  let _ = try ignore (List.assoc tag cs'); failwith "constructor redef in measure" with 
            Not_found -> () in
  Lightenv.add p (r :: cs) env

let transl_desc mlenv (c, (ps, rs)) =
  let c =  
    (Env.lookup_constructor (Longident.parse c) mlenv) in
  let tag = c.cstr_tag in
  let fr = F.fresh_without_vars mlenv c.cstr_res in
  let p = function
    F.Fsum(p, None, _, _) -> p
    | _ -> failwith "constructor result is not a constructed type?" in
  (p fr, (tag, ps, rs)) 

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
    F.Farrow (Some (Ty.Tpat_var a), b, f2) -> F.Farrow (Some (Ty.Tpat_var a), b, F.apply_refinement [funr a] f2)
  | F.Farrow (None, b, f2) -> 
      let a = Ident.create "x" in
        F.Farrow (Some (Ty.Tpat_var a), b, F.apply_refinement [funr a] f2)
  | _ -> failwith "not a fun in mk_fun"

let mk_tys e =
  let ty s =
    let sf = find_by_name s e in
    mk_fun s sf in
  List.map ty builtin_funs

let mk_pred v (_, _, ms) =
  let cm (s, e) = P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) in 
  P.big_and (List.map cm ms) 

let mk_qual c =
  let v = Path.mk_ident "v" in
  (Path.mk_ident "measure", v, mk_pred v c)

let mk_single_gd menv p vp (tag, _) =
  try Some (mk_pred vp (find_c p tag menv)) with 
    Not_found -> None

let mk_guards f e pat_exps =
  let vp = match e with 
      Ty.Texp_ident (p, _) -> p 
    | _ -> failwith "only match on idents supported; try normalizing" in
  let (p, rows) = match f with
      F.Fsum(p, None, constrs, _) -> (p, constrs)
    | _ -> assert false in 
    List.map (mk_single_gd !bms p vp) rows
