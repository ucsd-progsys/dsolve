(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)


module IM = Misc.IntMap
module Le = Liqenv
module Cd = Consdef

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
(***************************** Paths and Symbols *****************************)
(*****************************************************************************)

let str_to_path = Hashtbl.create 37
let path_to_str = Hashtbl.create 37

let str_of_path p =
  Misc.do_bimemo path_to_str str_to_path Path.unique_name p p
let sy_of_path p = Sy.of_string (str_of_path p)

let sy_of_qvar k =
  Sy.of_string ("k" ^ string_of_int k)

let qvar_of_sy sy =
  let s = Sy.to_string sy in
    int_of_string (String.sub s 1 (String.length s - 1))

let path_of_str s = 
  Hashtbl.find str_to_path s

let path_of_sy s =
  try path_of_str (Sy.to_string s) with
    Not_found ->
      (Printf.printf "\n%s\n" (Sy.to_string s);
      assert false)

let sy_of_persistent p = Sy.of_string (Path.name p)

(*****************************************************************************)
(*********************************** Sorts ***********************************)
(*****************************************************************************)

let rec fsort_of_dprover_t = function
  | Parsetree.Pprover_array (t1, t2) ->
      So.t_obj
  | Parsetree.Pprover_fun ts ->
      So.t_func 0 (List.map fsort_of_dprover_t ts)
  | Parsetree.Pprover_abs ("int") ->
      So.t_int
  | Parsetree.Pprover_abs ("bool") ->
      So.t_bool
  | Parsetree.Pprover_abs s ->
      So.t_obj

let rec dprover_t_of_fsort sort = 
  if So.is_bool sort then
    Parsetree.Pprover_abs("bool")
  else if So.t_int = sort then
    Parsetree.Pprover_abs("int")
  else if So.t_obj = sort then
    Parsetree.Pprover_abs("unint")
  else
    match So.func_of_t sort with
    | Some (ts, t) -> Parsetree.Pprover_fun (List.map dprover_t_of_fsort (ts @ [t]))
    | None -> assert false (* somehow we have encountered a pointer sort *)

(*****************************************************************************)
(**************************** Ops and RelsSorts ******************************)
(*****************************************************************************)

let f_of_dbop = function
  | P.Plus -> Ast.Plus
  | P.Minus -> Ast.Minus
  | P.Times -> Ast.Times
  | P.Div -> Ast.Div

let d_of_fbop = function
  | Ast.Plus -> P.Plus
  | Ast.Minus -> P.Minus
  | Ast.Times -> P.Times
  | Ast.Div -> P.Div

let f_of_dbrel = function
  | P.Eq -> Ast.Eq
  | P.Ne -> Ast.Ne
  | P.Gt -> Ast.Gt
  | P.Ge -> Ast.Ge
  | P.Lt -> Ast.Lt
  | P.Le -> Ast.Le

let d_of_fbrel = function
  | Ast.Eq -> P.Eq
  | Ast.Ne -> P.Ne
  | Ast.Gt -> P.Gt
  | Ast.Ge -> P.Ge
  | Ast.Lt -> P.Lt
  | Ast.Le -> P.Le

(*****************************************************************************)
(************************ Expressions and Predicates *************************)
(*****************************************************************************)

let rec f_of_dexpr = function
  | P.PInt i            -> Ast.eCon (Ast.Constant.Int i) 
  | P.Var p             -> Ast.eVar (sy_of_path p)
  | P.FunApp (p, es)    -> Ast.eApp (sy_of_path p, List.map f_of_dexpr es)
  | P.Binop (p1, b, p2) -> Ast.eBin (f_of_dexpr p1, f_of_dbop b, f_of_dexpr p2)
  | P.Field (p, e)      -> Ast.eFld (sy_of_path p, f_of_dexpr e)
  | P.Ite (b, e1, e2)   -> Ast.eIte (f_of_dpred b, f_of_dexpr e1, f_of_dexpr e2)

and f_of_dpred = function
  | P.True                -> Ast.pTrue
  | P.Atom (p1, r, p2)    -> Ast.pAtom (f_of_dexpr p1, f_of_dbrel r, f_of_dexpr p2)
  | P.Not p               -> Ast.pNot (f_of_dpred p)
  | P.And (p1, p2)        -> Ast.pAnd [f_of_dpred p1; f_of_dpred p2] 
  | P.Or (p1, p2)         -> Ast.pOr  [f_of_dpred p1; f_of_dpred p2]
  | P.Implies (p1, p2)    -> Ast.pImp (f_of_dpred p1, f_of_dpred p2) 
  | P.Boolexp p           -> Ast.pBexp (f_of_dexpr p)
  | P.Exists (vs, p)      ->
      let vs =
        List.map (fun (p, t) -> (Sy.of_string (str_of_path p), fsort_of_dprover_t t)) vs in
      Ast.pNot (Ast.pForall (vs, Ast.pNot (f_of_dpred p)))
  | P.Forall (vs, p)     ->
      let vs =
        List.map (fun (p, t) -> (Sy.of_string (str_of_path p), fsort_of_dprover_t t)) vs in
      Ast.pForall (vs, f_of_dpred p)
  | P.Iff (p1, p2) ->
      let p1, p2 = f_of_dpred p1, f_of_dpred p2 in 
      Ast.pAnd [Ast.pImp (p1, p2); Ast.pImp (p2, p1)]

let rec d_of_fexpr e =
  match Ast.Expression.unwrap e with  
  | Ast.Con (Ast.Constant.Int i) -> P.PInt i
  | Ast.Var v                  -> P.Var (path_of_sy v)
  | Ast.App (fn, rgs)          -> P.FunApp (path_of_sy fn, List.map d_of_fexpr rgs)
  | Ast.Bin (e1, bop, e2)      -> P.Binop (d_of_fexpr e1, d_of_fbop bop, d_of_fexpr e2)
  | Ast.Ite (p, e1, e2)        -> P.Ite (d_of_fpred p, d_of_fexpr e1, d_of_fexpr e2)
  | Ast.Fld (fn, e)            -> P.Field (path_of_sy fn, d_of_fexpr e)
  | _                        -> assert false

and d_of_fpred p =
  match Ast.Predicate.unwrap p with
  | Ast.True                   -> P.True
  | Ast.False                  -> P.Not (P.True)
  | Ast.And []                 -> P.True
  | Ast.And [p]                -> d_of_fpred p
  | Ast.And (p :: ps)          ->
      List.fold_left (fun a p -> P.And (d_of_fpred p, a)) (d_of_fpred p) ps
  | Ast.Or []                  -> P.Not (P.True)
  | Ast.Or [p]                 -> d_of_fpred p
  | Ast.Or (p :: ps)           ->
      List.fold_left (fun a p -> P.Or (d_of_fpred p, a)) (d_of_fpred p) ps
  | Ast.Not p                  -> P.Not (d_of_fpred p)
  | Ast.Imp (p1, p2)           -> P.Implies (d_of_fpred p1, d_of_fpred p2)
  | Ast.Bexp e                 -> P.Boolexp (d_of_fexpr e)
  | Ast.Atom (e1, rel, e2)     ->
      P.Atom (d_of_fexpr e1, d_of_fbrel rel, d_of_fexpr e2)
  | Ast.Forall (ss, p)         ->
      let ss = List.map (fun (s, st) -> (path_of_sy s, dprover_t_of_fsort st)) ss in 
      P.Forall (ss, d_of_fpred p)


(*****************************************************************************)
(******************************** Builtins ***********************************)
(*****************************************************************************)

let so_tag   = So.t_func 0 [So.t_int; So.t_int]
let vv_tag   = Sy.value_variable so_tag
let envt0    = [sy_of_path P.tag_function, Cf.make_reft vv_tag so_tag []]
               |> List.fold_left (fun env (n, r) -> SM.add n r env) SM.empty

let dvv      = Common.qual_test_var
let vv       = sy_of_path dvv


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

let fpred_of_dqual = fun s -> thd3 <+> P.apply_substs s <+> f_of_dpred 

let sub_of_dsubs =
  List.rev_map (fun (p, e) -> (sy_of_path p, f_of_dexpr e)) <+> Su.of_list

let refas_of_single_refinement (s, q) =
  [match q with
  | F.Qconst q -> Cf.Conc (fpred_of_dqual s q)
  | F.Qvar k -> Cf.Kvar (sub_of_dsubs s, sy_of_qvar k)]

let refas_of_refinement r =
  let p  = r |> Cd.refinement_preds (fun _ -> []) (P.Var dvv) 
             |> (P.big_and <+> f_of_dpred) in
  let ks = r |> List.map  (fun (s, (_,ks)) -> (sub_of_dsubs s, List.map sy_of_qvar ks))
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
    let x = sy_of_path p  in
    let r = reft_of_frame fr in
    SM.add x r e
  end env envt0

let true_envt_of_denv env =
  Liqenv.fold begin fun p fr env -> 
    (* let p = if Path.same p Common.qual_test_var then dvv else p in *)
    let x  = sy_of_path p  in
    let so = sort_of_frame fr in 
    let r  = Cf.make_reft (Sy.value_variable so) so [] in
    SM.add x r env
  end env envt0 

(*****************************************************************************)
(******************************* Constraints *********************************)
(*****************************************************************************)

(* API *)
let make_t env g fr r1 sr = 
  let env' = envt_of_denv env in
  let g'   = f_of_dpred g in
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
       |> Cf.sol_add s (sy_of_qvar k)
       |> snd
  end soln SM.empty

(* translates fixsoln into dsolve-land lazily: assuming that all value variables are dvv *)
let d_of_fsoln soln =
  IM.empty
  |> SM.fold begin fun k ps s -> 
      ps |> List.map (fun p -> (Path.mk_ident "pred", dvv, d_of_fpred p))
         |> Misc.flip (IM.add (qvar_of_sy k)) s
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
