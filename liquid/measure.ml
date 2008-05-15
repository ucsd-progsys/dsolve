open Types

module P = Predicate
module C = Constraint
module F = Frame
module Le = Lightenv

type mdef = (string * P.pexpr) 
type m = (constructor_tag * Path.t option list * mdef list) list 
type t = m Le.t

let builtins = [
  ("Some", ([], [("H", P.PInt(1))])); 
  ("None", ([], [("H", P.PInt(0))]));
]

let empty = (Le.empty: m Le.t)

let add (p, ((tag, _, _) as r)) env = 
  let cs = try Le.find p env with
             Not_found -> [] in
  let t (a, b, c) = (a, (b, c)) in
  let cs' = List.map t cs in
  let _ = try ignore(List.assoc tag cs'); failwith "constructor redef in measure" with 
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
let make_bms env = 
  let f g e h = add (g h) e in
  bms := List.fold_left (f (transl_desc env)) empty builtins 
