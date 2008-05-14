module P = Predicate
module C = Constraint
module F = Frame
module Le = Lightenv

let val_id = Path.mk_ident "_AA"
let val_var = P.Var val_id

type t = (constructor_tag * Path.t list * F.refinement) list Le.t

type b_meas = { meas_cname: string; meas_def: P.t }

let find_n tnm tag = Le.find 
(*let find_c cstr = ()


let add cstr = ()


let addn = Le.addn
let empty = Le.empty*)

let builtins = [
  ("Some", "H", P.PInt(1)); 
  ("None", "H", P.PInt(0));
]

let proc_m mlenv =
  let map_name c =  
    (Env.lookup_constructor (Longident.parse c) mlenv) in
  let p (c, m, e) = { meas_cname = c; meas_def = P.Atom(P.FunApp(m, [val_var]), P.Eq, e) } in
   List.map p builtins 

let bms = ref empty
let make_bms = bms := addn proc_m 
