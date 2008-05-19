open Types

module P = Predicate
module C = Constraint
module F = Frame
module Le = Lightenv

type mdef = (string * P.pexpr) 
type m = (constructor_tag * Path.t option list * mdef list) list 
type t = m Le.t

let builtins = [
  ("Some", ([], [("h", P.PInt(1))])); 
  ("None", ([], [("h", P.PInt(0))]));
]

let (empty: t) = Le.empty

let find_c p t e =
  List.find (fun (t', _, _) -> t' = t) (Le.find p e)

let add (p, ((tag, _, _) as r)) env = 
  let cs = try Le.find p env with
             Not_found -> [] in
  let t (a, b, c) = (a, (b, c)) in
  let cs' = List.map t cs in
  let _ = try ignore (List.assoc tag cs'); failwith "constructor redef in measure" with 
            Not_found -> () in
  Lightenv.add p (r :: cs) env

let transl_desc mlenv (c, (ps, rs)) =
  let c =  
    (Env.lookup_constructor (Longident.parse c) mlenv) in
  let tag = c.cstr_tag in
  let fr = F.fresh_without_vars mlenv c.cstr_res in
  let p = function
    F.Fsum(p, _, _) -> p
    | _ -> failwith "constructor result is not a constructed type?" in
  (p fr, (tag, ps, rs)) 

let bms = ref empty
let mk_bms env = 
  let f g e h = add (g h) e in
  bms := List.fold_left (f (transl_desc env)) empty builtins 

let mk_fun n f = 
    let funr a = P.Atom (P.Var x, P.Eq, P.FunApp(n, P.Var a)) in  
    match f with
      Farrow (a, b, f2) -> Farrow (a, b, F.append_refinement [funr a] fw)
    | _ -> failwith "not a fun in mk_fun"

let mk_tys env =
  let gl y = List.rev_append (snd (snd y)) in
  let funs = List.fold_left gl [] builtins in
  let nms = List.map fst funs in
  let mk_ty s = 
    let shp = Wf.get_expr_shape (List.hd s) in
    let f a = F.same_shape (Wf.get_expr_shape a) shp
    if List.for_all f s then mk_fun shp
    else failwith "conflicting shapes for measure" in
  let assc = List.map (fun x -> (x, List.assoc x funs)) nms in
   List.map 

let mk_pred v (_, _, ms) =
  let cm (s, e) = P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) in 
  P.big_and (List.map cm ms) 

let mk_qual c =
  let v = Path.mk_ident "v" in
  (Path.mk_ident "measure", v, mk_pred v c)
