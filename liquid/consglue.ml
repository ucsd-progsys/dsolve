module D = Constraint
module C = Common

module F = FixConstraint

module Pg = Predglue

module Pred = Predicate
module Fr = Frame

module A = Ast
module Asm = Ast.Symbol.SMap

(************************** REFINEMENTS ****************************)

let vv = Path.mk_persistent "VV"
let syvv = Pg.sy_of_path vv

let f_of_dsubs subs =
  List.map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) subs

let unify_vv_dqual vv (x, v, p) =
  let sub var = if Path.same var v then Pred.Var vv else Pred.Var var in
  (x, vv, Pred.map_vars sub p)

let unify_vv_dsreft vv ((subs, q) as sreft) =
  match q with
  | Fr.Qconst q -> (subs, Fr.Qconst (unify_vv_dqual vv q))
  | Fr.Qvar _ -> sreft

let unify_vv_dreft vv (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_vv_dqual vv q) cs, ks))

let fpred_of_dqual (_, _, p) = Pg.f_of_dpred p

let frefas_of_dsreft (subs, q) =
  let subs = f_of_dsubs subs in 
  [match q with
  | Fr.Qconst q -> F.Conc (fpred_of_dqual q)
  | Fr.Qvar k -> F.Kvar (subs, Pg.sy_of_path k)]

let frefas_of_dreft (subs, (cs, ks)) =
  let subs = f_of_dsubs subs in
  let cs = List.map (fun q -> F.Conc (fpred_of_dqual q)) cs in
  let ks = List.map (fun k -> F.Kvar (subs, Pg.sy_of_path k)) ks in
  List.append cs ks

(************************** ENVIRONMENTS ***************************)

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

(*let f_of_drenvt env =
  Lightenv.fold (fun p r -> Asm.add (Pg.sy_of_path p) (fpred_of_dreft r)) Asm.empty env
  *)

let f_of_denvt env =
  Lightenv.fold (fun p r e -> 
    let p = if Path.same p C.qual_test_var then vv else p in
    Asm.add (Pg.sy_of_path p) (freft_of_dframe r) e) env Asm.empty

(************************** CONSTRAINTS ****************************)

let f_of_drefcons envt = function
    (* type of vv is hanging out elsewhere *) 
(*  | D.SubRef (renvt, gd, r1, r2, id) -> 
    let gd = Pg.f_of_dpred (D.guard_predicate () gd) in
    let (r1, r2) = (unify_vv_dreft vv r1, unify_vv_dsreft vv r2) in
    let (r1, r2) = (f_of_dreft r1, f_of_dsreft r2) in
    assert false*)
    (* type of vv is is in envt *)
  | D.WFRef  (envt, r, id) -> 
    let vvs  = fsort_of_dframe (Lightenv.find C.qual_test_var envt) in
    let reft = F.make_reft syvv vvs (frefas_of_dsreft (unify_vv_dsreft vv r)) in
    let envt = f_of_denvt envt in
    F.make_wf envt reft id
  | _ -> assert false
