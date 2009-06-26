module D = DsolveConstraint

module F = FixConstraint

module Pg = PredGlue

module Pred = Predicate
module Fr = Frame


(************************** REFINEMENTS ****************************)

let vv = Path.mk_persistent "VV"

let f_of_dsubs subs =
  List.map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) subs

let unify_vv_dqual vv (x, v, p) =
  let sub var = if Path.same var v then vv else var in
  (x, vv, Pred.map_vars sub p)

let unify_vv_dsreft vv (subs, q) =
  match q with
  | QConst q -> (subs, unify_vv_dqual vv q)
  | QVar _ -> sreft

let unify_vv_dreft vv (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_vv_dqual vv q) cs, ks))

let fpred_of_dqual (_, _, p) = Pq.f_of_dqual p

let f_of_dsreft (subs, q) =
  let subs = f_of_dsubs subs in 
  match q with
  | QConst q -> F.Conc (Pq.fpred_of_dqual q)
  | QVar k -> F.Kvar (Pq.sy_of_path k)

let f_of_dreft (subs, (cs, ks)) =
  let subs = f_of_dsubs subs in
  let cs = List.map (fun q -> F.Conc (fpred_of_dqual q)) cs in
  let ks = List.map (fun q -> F.Kvar (subs, Pq.sy_of_path k)) ks in
  List.append cs ks

(************************** ENVIRONMENTS ***************************)

let rec string_of_frame = function
  | Fr.Fsum (p, _, _, _)
  | Fr.Frec (p, _, _) 
  | Fr.FVar (p, _, _, _) -> Pq.sy_of_path p
  | Fr.Farrow (_, f1, f2) -> (string_of_frame f1) ^ "->" ^ (string_of_frame f2)
  | Fr.Fabstract (p, [], id, _) ->
      Pq.sy_of_path p
  | Fr.Fabstract (p, params, id, _) ->
      "(" ^ (String.concat " " (List.map string_of_frame params)) ^ ") " ^ (Pq.sy_of_path p)

let rec fsort_of_dframe = function
  | Fr.Fsum (p, _, _, _)
  | Fr.Frec (p, _, _) 
  | Fr.FVar (p, _, _, _) -> F.Unint (Pq.sy_of_path p)
  | Fr.Farrow (_, f1, f2) -> F.Func (fsort_of_dframe f1, fsort_of_dframe f2)
  | (Fr.Fabstract (p, params, id, _)) as fr ->
      if Path.same Predef.path_bool p then F.Bool
      else if Path.same Predef.path_int p then F.Int
      else F.Unint (string_of_frame fr)

let f_of_d_envt env =
  Lightenv.map 
        
(************************** CONSTRAINTS ****************************)

let f_of_drefcons envt = function
    (* type of vv is hanging out elsewhere *) 
  | D.SubRef (renvt, gd, r1, r2, _) -> 
    let gd = Pg.f_of_dpred (D.guard_predicate () gd) in
    let (r1, r2) = (unify_vv_dreft vv r1, unify_vv_dsreft vv r2) in
    let (r1, r2) = (f_of_dreft r1, f_of_dsreft r2) in
    (* type of vv is is in envt *)
  | D.WFRef  (envt, r, _) -> 
    let r = unify_vv_dreft vv r in
    
