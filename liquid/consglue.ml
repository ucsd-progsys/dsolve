
module IM = Misc.IntMap
module Le = Liqenv

module Cd = Consdef

module Pg = Predglue
module P  = Predicate
module F  = Frame

module Cf = FixConstraint
module So = Ast.Sort
module Sy = Ast.Symbol
module Q  = Ast.Qualifier
module SM = Ast.Symbol.SMap
module Su = Ast.Subst

open Misc.Ops

(*****************************************************************************)
(******************************** Builtins ***********************************)
(*****************************************************************************)

let so_tag   = So.t_func 0 [So.t_int; So.t_int]
let vv_tag   = Sy.value_variable so_tag
let envt0    = [Pg.sy_of_path P.tag_function, Cf.make_reft vv_tag so_tag []]
               |> List.fold_left (fun env (n, r) -> SM.add n r env) SM.empty

let dvv      = Common.qual_test_var
let vv       = Pg.sy_of_path dvv


(*****************************************************************************)
(********************************* Frames ************************************)
(*****************************************************************************)

let rec sort_of_frame = function
  | F.Fsum (p, _, _) | F.Finductive (p, _, _, _, _) | F.Frec (p, _, _, _) | F.Fabstract (p, _, _, _) ->
      if Path.same Predef.path_bool p then So.t_bool 
      else So.t_int (* if Path.same Predef.path_int p then So.t_int 
      else (So.t_ptr (So.Loc (Path.unique_name p))) *) (* So.t_obj *) 
  | F.Farrow (_, _, _) as fr ->
      fr |> collapse |> So.t_func 0
  | F.Fvar (id, _, _, _) ->
      So.t_int
      (* So.t_ptr (So.Loc ("fv_"^(Common.ident_name id))) *) (* So.t_obj *)

and collapse = function
  | F.Farrow (_, f1, f2) -> 
      sort_of_frame f1 :: collapse f2
  | s -> [sort_of_frame s]

(*****************************************************************************)
(******************* Normalize to single value variable (why?) ***************)
(*****************************************************************************)

let unify_dqual (x, v, p) =
  let sub v' = if Path.same v v' then P.Var dvv else P.Var v' in
  (x, dvv, P.map_vars sub p)

let unify_dsreft ((subs, q) as sreft) =
  match q with
  | F.Qconst q -> (subs, F.Qconst (unify_dqual q))
  | F.Qvar _   -> sreft

let unify_drefexpr (subs, (cs, ks)) =
  (subs, (List.map (fun q -> unify_dqual q) cs, ks))

let unify_dreft = List.map unify_drefexpr 

(*****************************************************************************)
(******************************** Refinements ********************************)
(*****************************************************************************)

let fpred_of_dqual = fun s -> thd3 <+> P.apply_substs s <+> Pg.f_of_dpred 

let sub_of_dsubs =
  List.rev_map (fun (p, e) -> (Pg.sy_of_path p, Pg.f_of_dexpr e)) <+> Su.of_list

let refas_of_single_refinement (s, q) =
  [match q with
  | F.Qconst q -> Cf.Conc (fpred_of_dqual s q)
  | F.Qvar k -> Cf.Kvar (sub_of_dsubs s, Pg.sy_of_qvar k)]

let refas_of_refinement r =
  let p  = r |> Cd.refinement_preds (fun _ -> []) (P.Var dvv) 
             |> (P.big_and <+> Pg.f_of_dpred) in
  let ks = r |> List.map  (fun (s, (_,ks)) -> (sub_of_dsubs s, List.map Pg.sy_of_qvar ks))
             |> Misc.flap (fun (s, ks) -> List.map (fun k -> Cf.Kvar (s, k)) ks) in
  (Cf.Conc p) :: ks

let reft_of_single_refinement t sr =
  sr |> unify_dsreft 
     |> refas_of_single_refinement 
     |> Cf.make_reft vv t

let reft_of_refinement t r = 
  r |> unify_dreft 
    |> refas_of_refinement 
    |> Cf.make_reft vv t

let reft_of_frame fr =
  let t   = fr |> sort_of_frame in
  let ras = fr |> Frame.get_refinement |> (function Some r -> refas_of_refinement r | _ -> []) in
  Cf.make_reft vv t ras 

(*****************************************************************************)
(******************************* Environments ********************************)
(*****************************************************************************)

let envt_of_denv env = 
  Liqenv.fold begin fun p fr e ->
    let x = Pg.sy_of_path p  in
    let r = reft_of_frame fr in
    SM.add x r e
  end env envt0

let true_envt_of_denv env =
  Liqenv.fold begin fun p fr env -> 
    (* let p = if Path.same p Common.qual_test_var then dvv else p in *)
    let x  = Pg.sy_of_path p  in
    let so = sort_of_frame fr in 
    let r  = Cf.make_reft (Sy.value_variable so) so [] in
    SM.add x r env
  end env envt0 

(*****************************************************************************)
(******************************* Constraints *********************************)
(*****************************************************************************)

(* API *)
let make_t env g fr r1 sr: FixConstraint.t = 
  let env' = envt_of_denv env in
  let g'   = Pg.f_of_dpred (Cd.guard_predicate () g) in
  let t'   = sort_of_frame fr in
  let r1'  = reft_of_refinement t' r1 in
  let r2'  = reft_of_single_refinement t' sr in
  Cf.make_t env' g' r1' r2' None []

let make_wf env fr r id =
  let env' = true_envt_of_denv env          in
  let t'   = sort_of_frame fr               in
  let r'   = reft_of_single_refinement t' r in
  Cf.make_wf env' r' id

(* {{{
  
let vv  = Pg.sy_of_path dvv
let ftag   = (vv, So.t_func 0 [So.t_obj; So.t_int], [])
let inject_tag env = SM.add (Pg.sy_of_path P.tag_function) ftag env

 let reft_of_refinement = fun t r -> Cf.make_reft vv t (refas_of_refinement (unify_dreft r))
 let f_of_dreft     = fun fr r -> reft_of_refinement (sort_of_frame fr) r 

 let fsort_of_reft (_, s, _) = s

let f_of_drenvt env ftenv =
  Liqenv.fold begin fun p r env ->
    let s = Pg.sy_of_path p in
    SM.add s (reft_of_refinement (fsort_of_reft (SM.find s ftenv)) r) env
  end env ftenv

let f_of_dsubcon vvt fmax_envt = function
  | Cd.SubRef (renvt, gd, r1, r2, id) -> 
    let gd = Pg.f_of_dpred (Cd.guard_predicate () gd) in
    let (r1, r2) = (f_of_dreft vvt r1, reft_of_single_refinement vvt r2) in
    Some (Cf.make_t (f_of_drenvt renvt fmax_envt) gd r1 r2 id [])
  | _ -> None

let f_of_dsubcons fmax_envt cs =
  cs |> List.map (fun (vvt, _, c) -> f_of_dsubcon vvt fmax_envt c)
     |> Misc.maybe_list

let wf_of_dwfcon = function
  | fr, _, Cd.WFRef (env, r, id) -> 
      Some (make_wf env fr r id) 
  | _ -> None

}}} *)

(*****************************************************************************)
(********************************* Solutions *********************************)
(*****************************************************************************)

let f_of_dsoln soln =
  Cd.Sol.fold begin fun k qs s ->
    qs |> List.map (unify_dqual <+> fpred_of_dqual [])
       |> Cf.sol_add s (Pg.sy_of_qvar k)
       |> snd
  end soln SM.empty

(* translates fixsoln into dsolve-land lazily: assuming that all value variables are dvv *)
let d_of_fsoln soln =
  IM.empty
  |> SM.fold begin fun k ps s -> 
      ps |> List.map (fun p -> (Path.mk_ident "pred", dvv, Pg.d_of_fpred p))
         |> Misc.flip (IM.add (Pg.qvar_of_sy k)) s
     end soln
  |> Cd.sol_of_solmap

(****************************************************************************)
(********************************* Solver ***********************************)
(****************************************************************************)

(* API *)
let solver fname cs s =
  (* translate to fixpoint *)
  let fcs  = cs |> List.map (function (_,_,Cd.FixRef c) -> c | _ -> assertf "Cg.solver") in
  let fwfs = cs |> Misc.map_partial (function (fr, _, Cd.WFRef (env,r,id)) -> Some (make_wf env fr r id) | _ -> None) in
  let s    = f_of_dsoln s in
  let _    = Format.printf "@[InitSoln:@\n%a@]" FixConstraint.print_soln s in
  (* solve with fixpoint *)
  let me,_ = Solve.create [] SM.empty [] 0 [] fcs fwfs [] in
  let bn   = Miscutil.chop_extension_if_any fname in
  let _    = Solve.save (bn ^ ".ml.in.fq") me s in
  let s,_  = Solve.solve me s in
  let _    = Format.printf "@[FinSoln:@\n%a@]" FixConstraint.print_soln s in
  let _    = Solve.save (bn ^ ".ml.out.fq") me s in
  (* translate to dsolve *)
  d_of_fsoln s
