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
    F.Fconstr(p, _, _) -> p
    | _ -> failwith "constructor result is not a constructed type?" in
  (p fr, (tag, ps, rs)) 

let bms = ref empty
let mk_bms env = 
  let f g e h = add (g h) e in
  bms := List.fold_left (f (transl_desc env)) empty builtins 

let mk_pred v (_, _, ms) =
  let cm (s, e) = P.Atom(P.FunApp(s, [P.Var v]), P.Eq, e) in 
  P.big_and (List.map cm ms) 

let mk_qual c =
  let v = Path.mk_ident "v" in
  (Path.mk_ident "measure", v, mk_pred v c)
