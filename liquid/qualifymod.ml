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

open Asttypes
open Typedtree
open Parsetree
open Btype
open Types
open Constraint
open Longident
open Location
open Format

module P = Predicate
module C = Common
module Cf = Clflags
module B = Builtins
module Le = Lightenv
module F = Frame

(******************************************************************************)
(******************************* Error reporting ******************************)
(******************************************************************************)

type error =
  | NotSubtype of F.t * F.t
  | IllFormed of F.t
  | AssertMayFail

exception Error of Location.t * error
exception Errors of (Location.t * error) list

let make_frame_error s cstr =
  let rec error_rec cstr =
    match cstr.lc_orig with
      | Loc loc ->
        begin match cstr.lc_cstr with
          | SubFrame (_, _, f1, f2) ->
            (loc, NotSubtype (F.apply_solution s f1,  F.apply_solution s f2))
          | WFFrame (_,f) -> (loc, IllFormed (F.apply_solution s f))
        end
      | Assert loc -> (loc, AssertMayFail)
      | Cstr cstr -> error_rec cstr
  in error_rec cstr

let report_error ppf  = function
  | AssertMayFail ->
    fprintf ppf "@[Assertion may fail@]"
  | NotSubtype (f1, f2) ->
    fprintf ppf "@[@[%a@]@;<1 2>is not a subtype of@;<1 2>@[%a@]" F.pprint f1 F.pprint f2
  | IllFormed f ->
    fprintf ppf "@[Type %a is ill-formed" F.pprint f

let rec report_errors ppf = function
  | (l, e) :: es ->
    fprintf ppf "@[%a%a@\n@\n@]" Location.print l report_error e; report_errors ppf es
  | [] -> ()

(******************************************************************************)
(****************************** .annot generation *****************************)
(******************************************************************************)

module FrameLog = Map.Make(struct type t = Location.t
                                  let compare = compare end)
let flog = ref FrameLog.empty

let log_frame loc fr =
  if loc <> Location.none then 
    flog := FrameLog.add loc fr !flog

let framemap_apply_solution s fmap = FrameLog.map (F.apply_solution s) fmap

let dump_frame pp loc fr =
  if loc.loc_start <> Lexing.dummy_pos && loc.loc_end <> Lexing.dummy_pos then
    Stypes.print_position pp loc.loc_start;
    fprintf pp " ";
    Stypes.print_position pp loc.loc_end;
    fprintf pp "@.type(@.  ";
    F.pprint pp fr;
    fprintf pp "@.)@."

let dump_frames sourcefile fmap =
  if !Cf.dump_frames then
    let filename = Misc.chop_extension_if_any sourcefile ^ ".annot" in
    let pp = formatter_of_out_channel (open_out filename) in
      FrameLog.iter (dump_frame pp) fmap

(******************************************************************************)
(**************************** Constraint generation ***************************)
(******************************************************************************)

let label_constraint exp fc =
  let org = match exp.exp_desc with Texp_assert _ -> Assert exp.exp_loc | _ -> Loc exp.exp_loc in
    {lc_cstr = fc; lc_tenv = exp.exp_env; lc_orig = org; lc_id = fresh_fc_id()}

let expression_to_pexpr e =
  match e.exp_desc with
    | Texp_constant (Const_int n) -> P.PInt n
    | Texp_ident (id, _) -> P.Var id
    | _ -> P.Var (Path.mk_ident "dummy")

let expr_fresh = function
  | Texp_construct _ | Texp_assertfalse -> Frame.fresh_unconstrained
  | _ -> Frame.fresh

let rec constrain e env guard =
  let freshf = expr_fresh e.exp_desc e.exp_env e.exp_type in
  let desc_ty = (e.exp_desc, freshf) in
  let environment = (env, guard, freshf) in
  let (f, cstrs, rec_cstrs) =
    match desc_ty with
      | (Texp_constant const_typ, F.Fabstract(path, [], _)) -> constrain_constant path const_typ
      | (Texp_construct (cstrdesc, args), F.Fconstr _) -> constrain_constructed environment cstrdesc args e
      | (Texp_record (labeled_exprs, None), _) -> constrain_record environment labeled_exprs
      | (Texp_field (expr, label_desc), _) -> constrain_field environment expr label_desc
      | (Texp_ifthenelse (e1, e2, Some e3), _) -> constrain_if environment e1 e2 e3
      | (Texp_match (e, pexps, partial), _) -> constrain_match environment e pexps partial
      | (Texp_function ([(pat, e')], _), t) -> constrain_function environment t pat e'
      | (Texp_ident (id, _), F.Fabstract(_, [], _))
      | (Texp_ident (id, _), F.Fvar _) ->
          constrain_base_identifier environment id e
      | (Texp_ident (id, _), _) -> constrain_identifier environment id e.exp_env
      | (Texp_apply (e1, exps), _) -> constrain_application environment e1 exps
      | (Texp_let (recflag, bindings, body_exp), t) -> constrain_let environment recflag bindings body_exp
      | (Texp_array es, _) -> constrain_array environment es
      | (Texp_sequence (e1, e2), _) -> constrain_sequence environment e1 e2
      | (Texp_tuple es, _) -> constrain_tuple environment es
      | (Texp_assertfalse, _) -> constrain_assertfalse environment
      | (Texp_assert e, _) -> constrain_assert environment e
      | (_, f) ->
        fprintf err_formatter "@[Warning: Don't know how to constrain expression,
        structure:@ %a@ location:@ %a@]@.@." F.pprint f Location.print e.exp_loc; flush stderr;
        assert false
  in log_frame e.exp_loc f; (f, (List.map (label_constraint e) cstrs) @ rec_cstrs)

and constrain_constant path = function
  | Const_int n -> (B.mk_int [B.equality_qualifier (P.PInt n)], [], [])
  | Const_float _ -> (B.uFloat, [], [])
  | Const_char _ -> (B.uChar, [], [])
  | Const_string _ -> (B.uString, [], [])
  | _ -> assert false

and constrain_constructed (env, guard, f) cstrdesc args e =
  match f with
  | F.Fconstr (path, cstrs, _) ->
      let tag = cstrdesc.cstr_tag in
      let cstrref = match tag with
        | Cstr_constant n | Cstr_block n -> B.tag_refinement n
        | Cstr_exception _ -> assert false
      in
      let f = F.Fconstr (path, cstrs, cstrref) in
      let cstrargs = F.params_frames (List.assoc tag cstrs) in
      let (argframes, argcs) = constrain_subexprs env guard args in
        (f,
         WFFrame(env, f) :: (List.map2 (fun arg formal -> SubFrame(env, guard, arg, formal)) argframes cstrargs),
         argcs)
  | _ -> assert false

and constrain_record (env, guard, f) labeled_exprs =
  let compare_labels ({lbl_pos = n}, _) ({lbl_pos = m}, _) = compare n m in
  let (_, sorted_exprs) = List.split (List.sort compare_labels labeled_exprs) in
  let (p, ps) = match f with F.Fconstr(p, [(_, ps)], _) -> (p, ps) | _ -> assert false in
  let (fs, subexp_cs) = constrain_subexprs env guard sorted_exprs in
  let to_field (id, _, v) f = (id, f, v) in
  let field_qualifier (id, _, _) fexpr = B.field_eq_qualifier id (expression_to_pexpr fexpr) in
  let f = F.record_of_params p (List.map2 to_field ps fs) (F.mk_refinement [] (List.map2 field_qualifier ps sorted_exprs) []) in
    (f, [WFFrame (env, f)], subexp_cs)

and constrain_field (env, guard, _) expr label_desc =
  let (recframe, cstrs) = constrain expr env guard in
  let (fieldname, fieldframe) = match recframe with
    | F.Fconstr (_, [(_, ps)], _) -> (match List.nth ps label_desc.lbl_pos with (i, f, _) -> (i, f))
    | _ -> assert false
  in
  let pexpr = P.Field (fieldname, expression_to_pexpr expr) in
  let f = F.apply_refinement (B.equality_refinement pexpr) fieldframe in
    (f, [WFFrame (env, f)], cstrs)

and constrain_if (env, guard, f) e1 e2 e3 =
  let (f1, cstrs1) = constrain e1 env guard in
  let guardvar = Path.mk_ident "guard" in
  let env' = Le.add guardvar f1 env in
  let guard2 = (guardvar, true)::guard in
  let (f2, cstrs2) = constrain e2 env' guard2 in
  let guard3 = (guardvar, false)::guard in
  let (f3, cstrs3) = constrain e3 env' guard3 in
    (f,
    [WFFrame(env, f); SubFrame(env', guard2, f2, f); SubFrame(env', guard3, f3, f)],
    cstrs1 @ cstrs2 @ cstrs3)

and bind tenv env guard pat frame pexpr =
  let env = F.env_bind tenv env pat.pat_desc frame in
    if Pattern.is_deep pat.pat_desc then
      Le.add (Path.mk_ident "pattern")
        (B.mk_int [(Path.mk_ident "", Path.mk_ident "", Pattern.desugar_bind pat.pat_desc pexpr)])
        env
    else env

and constrain_case (env, guard, f) matchf matche (pat, e) =
  let env = bind e.exp_env env guard pat matchf matche in
  let (fe, subcs) = constrain e env guard in
    (SubFrame (env, guard, fe, f), subcs)

and constrain_match ((env, guard, f) as environment) e pexps partial =
  let (matchf, matchcstrs) = constrain e env guard in
  let cases = List.map (constrain_case environment matchf (expression_to_pexpr e)) pexps in
  let (cstrs, subcstrs) = List.split cases in
    (f, WFFrame (env, f) :: cstrs, List.concat (matchcstrs :: subcstrs))

and constrain_function (env, guard, f) t pat e' =
  match f with
    | (F.Farrow (_, f, unlabelled_f')) ->
      let env' = F.env_bind e'.exp_env env pat.pat_desc f in
      let (f'', cstrs) = constrain e' env' guard in
      let f' = F.label_like unlabelled_f' f'' in
      let f = F.Farrow (Some pat.pat_desc, f, f') in
        (f, [WFFrame (env, f); SubFrame (env', guard, f'', f')], cstrs)
    | _ -> assert false

and instantiate_id id f env tenv =
  let env_f =
    try Le.find id env
    with Not_found -> Frame.fresh_without_vars tenv ((Env.find_value id tenv).val_type)
  in F.instantiate env_f f

and constrain_base_identifier (env, _, f) id e =
  let refn =
    if Le.mem id env then B.equality_refinement (expression_to_pexpr e) else F.empty_refinement
  in (F.apply_refinement refn (instantiate_id id f env e.exp_env), [], [])

and constrain_identifier (env, guard, f) id tenv =
  let f = instantiate_id id f env tenv in (f, [WFFrame(env, f)], [])

and apply_once env guard (f, cstrs, subexp_cstrs) e = match (f, e) with
  | (F.Farrow (l, f, f'), (Some e2, _)) ->
    let (f2, e2_cstrs) = constrain e2 env guard in
    let f'' = match l with
      | Some pat ->
        List.fold_right F.apply_substitution (Pattern.bind_pexpr pat (expression_to_pexpr e2)) f'
          (* pmr: The soundness of this next line is suspect,
             must investigate (i.e., what if there's a var that might
             somehow be substituted that isn't because of this?  How
             does it interact w/ the None label rules for subtyping?) *)
      | _ -> f'
    in (f'', SubFrame (env, guard, f2, f) :: cstrs, e2_cstrs @ subexp_cstrs)
  | _ -> assert false

and constrain_application (env, guard, _) func exps =
  let (func_frame, func_cstrs) = constrain func env guard
  in List.fold_left (apply_once env guard) (func_frame, [], func_cstrs) exps

and constrain_let (env, guard, f) recflag bindings body =
  let (env', cstrs1) = constrain_bindings env guard recflag bindings in
  let (body_frame, cstrs2) = constrain body env' guard in
  match body.exp_desc with
    | Texp_let _ -> (body_frame, [WFFrame (env, body_frame)], cstrs1 @ cstrs2)
    | _ ->
      let f = F.label_like f body_frame in
        (f, [WFFrame (env, f); SubFrame (env', guard, body_frame, f)], cstrs1 @ cstrs2)

and constrain_array (env, guard, f) elements =
  let (f, fa) =
    (match f with
      | F.Fabstract(p, ([(_, fa, _)] as ps), _) ->
          (F.Fabstract(p, ps, B.size_lit_refinement(List.length elements)), fa)
      | _ -> assert false) in
  let list_rec (fs, c) e = (fun (f, cs) -> (f::fs, cs @ c)) (constrain e env guard) in
  let (fs', sub_cs) = List.fold_left list_rec ([], []) elements in
  let mksub b a = SubFrame(env, guard, a, b) in
    (f, WFFrame(env, f) :: List.map (mksub fa) fs', sub_cs)

and constrain_sequence (env, guard, _) e1 e2 =
  let (_, cs1) = constrain e1 env guard in
  let (f, cs2) = constrain e2 env guard in (f, [], cs1 @ cs2)

and elem_qualifier fexpr n =
  B.proj_eq_qualifier n (expression_to_pexpr fexpr)

and constrain_tuple (env, guard, _) es =
  let (fs, subexp_cs) = constrain_subexprs env guard es in
  let f = F.tuple_of_frames fs (F.mk_refinement [] (Misc.mapi elem_qualifier es) []) in
    (f, [WFFrame(env, f)], subexp_cs)

and constrain_assertfalse (env, _, f) =
  (f, [WFFrame (env, f)], [])

and constrain_assert (env, guard, _) e =
  let (f, cstrs) = constrain e env guard in
  let guardvar = Path.mk_ident "assert_guard" in
  let env = Le.add guardvar f env in
  let assert_qualifier =
    (Path.mk_ident "assertion",
     Path.mk_ident "null",
     P.equals (B.tag(P.Var guardvar), P.int_true))
  in (B.uUnit, [SubFrame (env, guard, B.mk_int [], B.mk_int [assert_qualifier])], cstrs)

and constrain_and_bind guard (env, cstrs) (pat, e) =
  let (f, cstrs') = constrain e env guard in
  let env = bind e.exp_env env guard pat f (expression_to_pexpr e) in
    (env, cstrs @ cstrs')

and bind_all bindings fs tenv env guard =
  List.fold_right2 (fun (p, e, px) f env -> bind tenv env guard p f px) bindings fs env

and constrain_bindings env guard recflag bindings =
  match recflag with
  | Default | Nonrecursive -> List.fold_left (constrain_and_bind guard) (env, []) bindings
  | Recursive ->
    let tenv = (snd (List.hd bindings)).exp_env in
    let (_, exprs) = List.split bindings in
    let bindings = List.map (fun (p, e) -> (p, e, expression_to_pexpr e)) bindings in

    (* We need to figure out all the frames' labels before we can properly bind them. *)
    let unlabeled_frames = List.map (fun e -> F.fresh e.exp_env e.exp_type) exprs in
    let unlabeled_env = bind_all bindings unlabeled_frames tenv env guard in
    let (label_frames, _) = constrain_subexprs unlabeled_env guard exprs in
    let binding_frames = List.map2 F.label_like unlabeled_frames label_frames in
            
    (* Redo constraints now that we know what the right labels are *)
    let bound_env = bind_all bindings binding_frames tenv env guard in
    let (found_frames, subexp_cstrs) = constrain_subexprs bound_env guard exprs in

    let make_cstr fc = {lc_cstr = fc; lc_tenv = tenv; lc_orig = Loc Location.none; lc_id = fresh_fc_id ()} in
    let build_found_frame_cstr_list cs found_frame binding_frame =
      make_cstr (WFFrame (bound_env, binding_frame)) ::
      make_cstr (SubFrame (bound_env, guard, found_frame, binding_frame)) :: cs
    in (bound_env, (List.fold_left2 build_found_frame_cstr_list [] found_frames binding_frames) @ subexp_cstrs)

and constrain_subexprs env guard es =
  List.fold_right (fun e (fs, cs) -> let (f, cs') = constrain e env guard in (f :: fs, cs' @ cs)) es ([], [])

let constrain_structure initfenv initquals str =
  let rec constrain_rec quals fenv cstrs = function
    | [] -> (quals, fenv, cstrs)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs') = constrain exp fenv []
        in constrain_rec quals fenv (cstrs' @ cstrs) srem
    | (Tstr_qualifier (name, (valu, pred))) :: srem ->
        let quals = (Path.Pident name, Path.Pident valu, pred) :: quals in
          constrain_rec quals fenv cstrs srem
    | (Tstr_value (recflag, bindings))::srem ->
        let (fenv, cstrs') = constrain_bindings fenv [] recflag bindings
        in constrain_rec quals fenv (cstrs @ cstrs') srem
    | (Tstr_type(_))::srem ->
        constrain_rec quals fenv cstrs srem
    | _ -> assert false
  in constrain_rec initquals initfenv [] str

(******************************************************************************)
(***************************** Qualifying modules *****************************)
(******************************************************************************)

let pre_solve () = 
  C.cprintf C.ol_solve_master "@[##solve##@\n@]"; Bstats.reset ()

let post_solve () = 
  if C.ck_olev C.ol_timing then
    (Printf.printf "##time##\n"; Bstats.print stdout "\nTime to solve constraints:\n";
    Printf.printf "##endtime##\n"; (*TheoremProver.dump_simple_stats ()*))

let lbl_dummy_cstr env c =
  { lc_cstr = c; lc_tenv = env; lc_orig = Loc (Location.none); lc_id = fresh_fc_id () }

let mfm fenv p f = 
  if Le.mem p fenv
  then
    let f' = Le.find p fenv in
    Some (SubFrame (fenv, [], f', F.label_like f f'))
  else
    None 
 
let qualify_implementation sourcefile fenv ifenv env qs str =
  let (qs, fenv, cs) = constrain_structure fenv qs str in
  let cs = (List.map (lbl_dummy_cstr env) (Le.maplistfilter (mfm fenv) ifenv)) @ cs in
  let _ = pre_solve () in
  let (s,cs) = Bstats.time "solving" (solve qs) cs in
  let _ = post_solve () in
  let _ = dump_frames sourcefile (framemap_apply_solution s !flog) in
  match cs with [] -> () | _ ->
    (Printf.printf "Errors encountered during type checking:\n\n";
    flush stdout; raise (Errors(List.map (make_frame_error s) cs)))
