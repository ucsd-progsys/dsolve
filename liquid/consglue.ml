module D = Constraint
module C = Common

module F = FixConstraint

module Pg = Predglue

module Pred = Predicate
module Fr = Frame

module A = Ast
module S = A.Sort
module Sy = A.Symbol
module Q = A.Qualifier
module Asm = Sy.SMap
module Le = Lightenv

let dvv = C.qual_test_var
let dsyvv = Pg.sy_of_path dvv

let empty_sol = (fun _ -> [])

(************************** FRAMES **********************************)

let untStr = "unint"
let untSrt = S.Unint untStr

let tagSt = S.Func [untSrt; S.Int]
let tagSy = Pg.sy_of_path Pred.tag_function
let ftag  = (dsyvv, tagSt, [])

let inject_tag env = Asm.add tagSy ftag env

let rec string_of_frame = function
  | Fr.Fsum (p, _, _)
  | Fr.Finductive (p, _, _, _, _, _)
  | Fr.Frec (p, _, _, _) 
  | Fr.Fabstract (p, _, _, _) ->
      if Path.same Predef.path_bool p then "bool" else
      if Path.same Predef.path_int p then "int" else untStr
  | Fr.Farrow (_, f1, f2) -> (string_of_frame f1) ^ "->" ^ (string_of_frame f2)
  | Fr.Fvar (_, _, _, _) -> untStr

let rec fsort_of_dframe fr =
  match fr with
  | Fr.Fsum (p, _, _)
  | Fr.Finductive (p, _, _, _, _, _)
  | Fr.Frec (p, _, _, _) 
  | Fr.Fabstract (p, _, _, _) ->
      if Path.same Predef.path_bool p then S.Bool else
      if Path.same Predef.path_int p then S.Int else S.Unint (string_of_frame fr)
  | Fr.Farrow (_, f1, f2) -> S.Func (collapse fr)
  | Fr.Fvar (_, _, _, _) -> S.Unint (string_of_frame fr)

and collapse = function
  | Fr.Farrow (_, f1, f2) -> fsort_of_dframe f1 :: collapse f2
  | s -> [fsort_of_dframe s]

  (* returns an empty but appropriately sorted reft
   * that is only appropriate for wf cons *)
let freft_of_dframe fr = F.make_reft dsyvv (fsort_of_dframe fr) []


(************************** REFINEMENTS ****************************)

let f_of_dsubs subs =
  List.rev_map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) subs

let unify_dqual (x, v, p) =
  let sub var = if Path.same var v then Pred.Var dvv else Pred.Var var in
  (x, dvv, Pred.map_vars sub p)

let unify_dsreft ((subs, q) as sreft) =
  match q with
  | Fr.Qconst q -> (subs, Fr.Qconst (unify_dqual q))
  | Fr.Qvar _ -> sreft

let unify_drefexpr (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_dqual q) cs, ks))

let unify_dreft reft = List.map unify_drefexpr reft

let fpred_of_dqual subs (_, _, p) =
  Pg.f_of_dpred (Pred.apply_substs subs p)

let dqual_of_fpred p =
  (Path.mk_ident "pred", dvv, Pg.d_of_fpred p)

let frefas_of_dsreft (subs, q) =
  [match q with
  | Fr.Qconst q -> F.Conc (fpred_of_dqual subs q)
  | Fr.Qvar k -> F.Kvar (f_of_dsubs subs, Pg.sy_of_path k)]

let frefas_of_drefexpr (subs, (_, ks)) =
  List.map (fun k -> F.Kvar (f_of_dsubs subs, Pg.sy_of_path k)) ks

let frefas_of_dreft reft =
  let consts =
    Pg.f_of_dpred (Pred.big_and (D.refinement_preds empty_sol (Pred.Var dvv) reft)) in
  (F.Conc consts) :: C.flap frefas_of_drefexpr reft

let freft_of_dsreft vvt sreft =
  F.make_reft dsyvv (fsort_of_dframe vvt) (frefas_of_dsreft (unify_dsreft sreft))

let f_of_sortdreft vvs reft =
  F.make_reft dsyvv vvs (frefas_of_dreft (unify_dreft reft))

let f_of_dreft vvt reft =
  f_of_sortdreft (fsort_of_dframe vvt) reft


(*************************** QUALIFIERS ****************************)

  (* qualifier translation before dsolve does wf is _incomplete_
   * because dsolve does not type the value variable.
   * use dsolve's initial solution for best results. *)
let f_of_dqual q =
  let ss = [S.Int; S.Bool; untSrt] in (* this is the crux *)
  List.map (fun s -> Q.create (Some dsyvv) s (fpred_of_dqual [] (unify_dqual q))) ss 

let f_of_dquals qs =
  C.flap f_of_dqual qs

(************************** ENVIRONMENTS ***************************)

let fsort_of_reft (_, s, _) = s

let f_of_drenvt env ftenv =
  Lightenv.fold (fun p r e ->
    let s = Pg.sy_of_path p in
    Asm.add s (f_of_sortdreft (fsort_of_reft (Asm.find s ftenv)) r) e) env ftenv

let f_of_denvt env =
  Lightenv.fold (fun p fr e -> 
    let p = if Path.same p C.qual_test_var then dvv else p in
    Asm.add (Pg.sy_of_path p) (freft_of_dframe fr) e) env Asm.empty

(************************** CONSTRAINTS ****************************)

let f_of_dsubcon vvt fmax_envt = function
  | D.SubRef (renvt, gd, r1, r2, id) -> 
    let gd = Pg.f_of_dpred (D.guard_predicate () gd) in
    let (r1, r2) = (f_of_dreft vvt r1, freft_of_dsreft vvt r2) in
    Some (F.make_t (f_of_drenvt renvt fmax_envt) gd r1 r2 id [])
  | _ -> None

let f_of_dsubcons fmax_envt cons =
  C.maybe_list (List.map (fun (vvt, _, con) -> f_of_dsubcon vvt fmax_envt con) cons)

let f_of_dwfcon vvt = function
  | D.WFRef  (envt, r, id) -> 
    let reft = freft_of_dsreft vvt r in
    Some (F.make_wf (f_of_denvt envt) reft id)
  | _ -> None

let f_of_dwfcons cons =
  C.maybe_list (List.map (fun (vvt, _, con) -> f_of_dwfcon vvt con) cons)

(************************** SOLUTIONS ******************************)

let f_of_dsoln soln =
  let p q = fpred_of_dqual [] (unify_dqual q) in
  D.Sol.fold (fun k qs s ->
    snd (F.sol_add s (Pg.sy_of_path k) (List.map p qs))) soln Asm.empty

 (* translates fixsoln into dsolve-land lazily:
  * assuming that all value variables are dvv *)
let d_of_fsoln soln =
  let q = dqual_of_fpred in
  let rv = 
    Asm.fold (fun k ps s ->
      Le.add (Pg.path_of_sy k) (List.map q ps) s) soln Le.empty in
  D.sol_of_solmap rv
