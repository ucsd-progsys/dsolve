

module IM = Misc.IntMap

module Le = Liqenv
module Cl = Constraint
module Pg = Predglue
module P  = Predicate
module Fr = Frame

module Cf = FixConstraint
module So = Ast.Sort
module Sy = Ast.Symbol
module Q  = Ast.Qualifier
module SM = Ast.Symbol.SMap
module Su = Ast.Subst

open Misc.Ops

let dvv = Common.qual_test_var
let dsyvv = Pg.sy_of_path dvv
let empty_sol = (fun _ -> [])

(********************************************************************)
(************************** FRAMES **********************************)
(********************************************************************)

let untStr = "unint"
let untSrt = So.t_obj

let tagSt = So.t_func 0 [untSrt; So.t_int]
let tagSy = Pg.sy_of_path P.tag_function
let ftag  = (dsyvv, tagSt, [])

let inject_tag env = SM.add tagSy ftag env

let rec string_of_frame = function
  | Fr.Fsum (p, _, _)
  | Fr.Finductive (p, _, _, _, _)
  | Fr.Frec (p, _, _, _) 
  | Fr.Fabstract (p, _, _, _) ->
      if Path.same Predef.path_bool p then "bool" else
      if Path.same Predef.path_int p then "int" else untStr
  | Fr.Farrow (_, f1, f2) -> (string_of_frame f1) ^ "->" ^ (string_of_frame f2)
  | Fr.Fvar (_, _, _, _) -> untStr

let rec fsort_of_dframe fr =
  match fr with
  | Fr.Fsum (p, _, _)
  | Fr.Finductive (p, _, _, _, _)
  | Fr.Frec (p, _, _, _) 
  | Fr.Fabstract (p, _, _, _) ->
      if Path.same Predef.path_bool p then So.t_bool else
      if Path.same Predef.path_int p then So.t_int else So.t_obj
  | Fr.Farrow (_, f1, f2) ->
      let params = collapse fr in
      So.t_func 0 params
  | Fr.Fvar (_, _, _, _) -> So.t_obj
and collapse = function
  | Fr.Farrow (_, f1, f2) -> fsort_of_dframe f1 :: collapse f2
  | s -> [fsort_of_dframe s]

  (* returns an empty but appropriately sorted reft
   * that is only appropriate for wf cons *)
let freft_of_dframe fr = Cf.make_reft dsyvv (fsort_of_dframe fr) []


(************************** REFINEMENTS ****************************)

let f_of_dsubs subs =
  subs |> List.rev_map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) 
       |> Su.of_list

let unify_dqual (x, v, p) =
  let sub var = if Path.same var v then P.Var dvv else P.Var var in
  (x, dvv, P.map_vars sub p)

let unify_dsreft ((subs, q) as sreft) =
  match q with
  | Fr.Qconst q -> (subs, Fr.Qconst (unify_dqual q))
  | Fr.Qvar _ -> sreft

let unify_drefexpr (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_dqual q) cs, ks))

let unify_dreft reft = List.map unify_drefexpr reft

let fpred_of_dqual subs (_, _, p) =
  Pg.f_of_dpred (P.apply_substs subs p)

let dqual_of_fpred p =
  (Path.mk_ident "pred", dvv, Pg.d_of_fpred p)

let frefas_of_dsreft (subs, q) =
  [match q with
  | Fr.Qconst q -> Cf.Conc (fpred_of_dqual subs q)
  | Fr.Qvar k -> Cf.Kvar (f_of_dsubs subs, Pg.sy_of_qvar k)]

let frefas_of_drefexpr (subs, (_, ks)) =
  List.map (fun k -> Cf.Kvar (f_of_dsubs subs, Pg.sy_of_qvar k)) ks

let frefas_of_dreft reft =
  let consts =
    Pg.f_of_dpred (P.big_and (Cl.refinement_preds empty_sol (P.Var dvv) reft)) in
  (Cf.Conc consts) :: Misc.flap frefas_of_drefexpr reft

let freft_of_dsreft vvt sreft =
  Cf.make_reft dsyvv (fsort_of_dframe vvt) (frefas_of_dsreft (unify_dsreft sreft))

let f_of_sortdreft vvs reft =
  Cf.make_reft dsyvv vvs (frefas_of_dreft (unify_dreft reft))

let f_of_dreft vvt reft =
  f_of_sortdreft (fsort_of_dframe vvt) reft


(*************************** QUALIFIERS ****************************)

(* qualifier translation before dsolve does wf is _incomplete_
 * because dsolve does not type the value variable.
 * use dsolve's initial solution for best results. *)

let f_of_dqual q =
  let ss = [So.t_int; So.t_bool; untSrt] in (* this is the crux *)
  List.map (fun s -> Q.create dsyvv s (fpred_of_dqual [] (unify_dqual q))) ss 

let f_of_dquals qs =
  Misc.flap f_of_dqual qs

(************************** ENVIRONMENTS ***************************)

let fsort_of_reft (_, s, _) = s

let f_of_drenvt env ftenv =
  Liqenv.fold (fun p r e ->
    let s = Pg.sy_of_path p in
    SM.add s (f_of_sortdreft (fsort_of_reft (SM.find s ftenv)) r) e) env ftenv

let f_of_denvt env =
  Liqenv.fold (fun p fr e -> 
    let p = if Path.same p Common.qual_test_var then dvv else p in
    SM.add (Pg.sy_of_path p) (freft_of_dframe fr) e) env SM.empty

(************************** CONSTRAINTS ****************************)

let f_of_dsubcon vvt fmax_envt = function
  | Cl.SubRef (renvt, gd, r1, r2, id) -> 
    let gd = Pg.f_of_dpred (Cl.guard_predicate () gd) in
    let (r1, r2) = (f_of_dreft vvt r1, freft_of_dsreft vvt r2) in
    Some (Cf.make_t (f_of_drenvt renvt fmax_envt) gd r1 r2 id [])
  | _ -> None

let f_of_dsubcons fmax_envt cons =
  Misc.maybe_list (List.map (fun (vvt, _, con) -> f_of_dsubcon vvt fmax_envt con) cons)

let f_of_dwfcon vvt = function
  | Cl.WFRef  (envt, r, id) -> 
    let reft = freft_of_dsreft vvt r in
    Some (Cf.make_wf (f_of_denvt envt) reft id)
  | _ -> None

let f_of_dwfcons cons =
  Misc.maybe_list (List.map (fun (vvt, _, con) -> f_of_dwfcon vvt con) cons)

(************************** SOLUTIONS ******************************)

let f_of_dsoln soln =
  let p q = fpred_of_dqual [] (unify_dqual q) in
  Cl.Sol.fold (fun k qs s ->
    snd (Cf.sol_add s (Pg.sy_of_qvar k) (List.map p qs))) soln SM.empty

(* translates fixsoln into dsolve-land lazily: assuming that all value variables are dvv *)
let d_of_fsoln soln =
  let q = dqual_of_fpred in
  let rv = 
    SM.fold (fun k ps s ->
      IM.add (Pg.qvar_of_sy k) (List.map q ps) s) soln IM.empty in
  Cl.sol_of_solmap rv
