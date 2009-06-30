module D = Constraint
module C = Common

module F = FixConstraint

module Pg = Predglue

module Pred = Predicate
module Fr = Frame

module A = Ast
module Asm = Ast.Symbol.SMap

let vv = Path.mk_persistent "VV"
let syvv = Pg.sy_of_path vv


(************************** FRAMES **********************************)

let rec string_of_frame = function
  | Fr.Fsum (p, _, _, _)
  | Fr.Frec (p, _, _) 
  | Fr.Fvar (p, _, _, _) -> Pg.str_of_path p
  | Fr.Farrow (_, f1, f2) -> (string_of_frame f1) ^ "->" ^ (string_of_frame f2)
  | Fr.Fabstract (p, [], id, _) ->
      Pg.str_of_path p
  | Fr.Fabstract (p, params, id, _) ->
      "(" ^ (String.concat " " (List.map (fun (_, f, _) -> string_of_frame f) params)) ^ ") " ^ (Pg.str_of_path p)

let rec fsort_of_dframe = function
  | Fr.Fsum (p, _, _, _)
  | Fr.Frec (p, _, _) 
  | Fr.Fvar (p, _, _, _) -> Ast.Sort.Unint (Pg.str_of_path p)
  | Fr.Farrow (_, f1, f2) -> Ast.Sort.Func [fsort_of_dframe f1; fsort_of_dframe f2]
  | (Fr.Fabstract (p, params, id, _)) as fr ->
      if Path.same Predef.path_bool p then Ast.Sort.Bool
      else if Path.same Predef.path_int p then Ast.Sort.Int
      else Ast.Sort.Unint (string_of_frame fr)

  (* returns an empty but appropriately sorted reft
   * that is only appropriate for wf cons *)
let freft_of_dframe fr = F.make_reft syvv (fsort_of_dframe fr) []


(************************** REFINEMENTS ****************************)

let f_of_dsubs subs =
  List.map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) subs

let unify_vv_dqual vv (x, v, p) =
  let sub var = if Path.same var v then Pred.Var vv else Pred.Var var in
  (x, vv, Pred.map_vars sub p)

let unify_vv_dsreft vv ((subs, q) as sreft) =
  match q with
  | Fr.Qconst q -> (subs, Fr.Qconst (unify_vv_dqual vv q))
  | Fr.Qvar _ -> sreft

let unify_vv_drefexpr vv (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_vv_dqual vv q) cs, ks))

let unify_vv_dreft vv reft = List.map (unify_vv_drefexpr vv) reft

let fpred_of_dqual (_, _, p) = Pg.f_of_dpred p

let frefas_of_dsreft (subs, q) =
  let subs = f_of_dsubs subs in 
  [match q with
  | Fr.Qconst q -> F.Conc (fpred_of_dqual q)
  | Fr.Qvar k -> F.Kvar (subs, Pg.sy_of_path k)]

let frefas_of_drefexpr ((subs, (cs, ks)): Fr.refexpr) : F.refa list =
  let subs = f_of_dsubs subs in
  let cs = List.map (fun q -> F.Conc (fpred_of_dqual q)) cs in
  let ks = List.map (fun k -> F.Kvar (subs, Pg.sy_of_path k)) ks in
  List.append cs ks

let frefas_of_dreft : Fr.refinement -> F.refa list = (C.flap frefas_of_drefexpr)

let f_of_dreft vvt reft =
  F.make_reft syvv (fsort_of_dframe vvt) (frefas_of_dreft (unify_vv_dreft vv reft))

let freft_of_dsreft vvt sreft =
  F.make_reft syvv (fsort_of_dframe vvt) (frefas_of_dsreft (unify_vv_dsreft vv sreft))


(************************** ENVIRONMENTS ***************************)

let f_of_drenvt env tenv =
  Lightenv.fold (fun p r e ->
    Asm.add (Pg.sy_of_path p) (f_of_dreft (Lightenv.find p tenv) r) e) env Asm.empty

let f_of_denvt env =
  Lightenv.fold (fun p r e -> 
    let p = if Path.same p C.qual_test_var then vv else p in
    Asm.add (Pg.sy_of_path p) (freft_of_dframe r) e) env Asm.empty


(************************** CONSTRAINTS ****************************)

let f_of_drefcon vvt max_envt = function
  | D.SubRef (renvt, gd, r1, r2, id) -> 
    let gd = Pg.f_of_dpred (D.guard_predicate () gd) in
    (*let vvt = Lightenv.find C.qual_test_var max_envt in*)
    let (r1, r2) = (f_of_dreft vvt r1, freft_of_dsreft vvt r2) in
    Some (F.make_t (f_of_drenvt renvt max_envt) gd r1 r2 id)
  | _ -> None

let f_of_drefcons max_envt cons = C.maybe_list (List.map (fun (vvt, _, con) -> f_of_drefcon vvt max_envt con) cons)

let f_of_dwfcon vvt max_envt = function
  | D.WFRef  (envt, r, id) -> 
    let reft = freft_of_dsreft (*Lightenv.find C.qual_test_var envt*) vvt r in
    Some (F.make_wf (f_of_denvt envt) reft id)
  | _ -> None

let f_of_dwfcons vvt max_envt cons = C.maybe_list (List.map (f_of_dwfcon vvt max_envt) cons)

(************************** SOLUTIONS ******************************)

let f_of_dsol sol =
  let p q = fpred_of_dqual (unify_vv_dqual vv q) in
  D.Sol.fold (fun k qs s ->
    snd (F.sol_add s (Pg.sy_of_path k) (List.map p qs))) sol Asm.empty

(*let d_of_fsol sol =
  Asm.fold (fun k qs s ->
    D.Sol.replace (Pg.path_of_sy) (Lis
    *)
