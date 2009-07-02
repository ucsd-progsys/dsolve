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

module P        = Predicate
module C        = Common
module Cf       = Clflags
module B        = Builtins
module Le       = Lightenv
module F        = Frame
module M        = Measure
module WF       = Wellformed

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

let reset_framelog () =
  flog := FrameLog.empty

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
    let filename = Miscutil.chop_extension_if_any sourcefile ^ ".annot" in
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

let rec constrain e env guard =
  let _ = Frame.initialize_type_expr_levels e.exp_type in
  let freshf = Frame.fresh e.exp_env e.exp_type in
  let environment = (env, guard, freshf) in
  let (f, cstrs, rec_cstrs) =
   try
    match (e.exp_desc, freshf) with
      | (Texp_constant const_typ, F.Fabstract(path, [], _, _)) -> constrain_constant path const_typ
      | (Texp_construct (cstrdesc, args), F.Fsum _) -> constrain_constructed environment cstrdesc args e
      | (Texp_record (labeled_exprs, None), _) -> constrain_record environment labeled_exprs
      | (Texp_field (expr, label_desc), _) -> constrain_field environment expr label_desc
      | (Texp_setfield (expr, label_desc, expr'), _) -> constrain_setfield environment expr label_desc expr'
      | (Texp_ifthenelse (e1, e2, Some e3), _) -> constrain_if environment e1 e2 e3
      | (Texp_match (e, pexps, partial), _) -> constrain_match environment e pexps partial
      | (Texp_function ([(pat, e')], _), _) -> constrain_function environment pat e'
      | (Texp_ident (id, _), F.Fsum(_, _, _, _))
      | (Texp_ident (id, _), F.Fabstract(_, _, _, _))
      | (Texp_ident (id, _), F.Fvar _) ->
          constrain_base_identifier environment id e
      | (Texp_ident (id, _), _) -> constrain_identifier environment id e.exp_env
      | (Texp_apply (e1, exps), _) -> constrain_application environment e1 exps
      | (Texp_let (recflag, bindings, body_exp), t) -> constrain_let environment recflag bindings body_exp
      | (Texp_array es, _) -> constrain_array environment e.exp_env es
      | (Texp_sequence (e1, e2), _) -> constrain_sequence environment e1 e2
      | (Texp_tuple es, _) -> constrain_tuple environment es
      | (Texp_assertfalse, _) -> constrain_assertfalse environment e.exp_env e.exp_type
      | ((Texp_assert e) as form, _)
      | ((Texp_assume e) as form, _) ->
          constrain_guard environment form e
      | (_, f) ->
        fprintf err_formatter "@[Warning: Don't know how to constrain expression,
        structure:@ %a@ location:@ %a@]@.@." F.pprint f Location.print e.exp_loc; flush stderr;
        assert false
   with Failure s -> printf "@[Failure:@ %s:@ %a@]" s F.pprint freshf; assert false
  in log_frame e.exp_loc f; (f, (List.map (label_constraint e) cstrs) @ rec_cstrs)

and constrain_constant path = function
  | Const_int n    -> (B.mk_int [B.equality_qualifier (P.PInt n)], [], [])
  | Const_float _  -> (B.uFloat, [], [])
  | Const_char _   -> (B.uChar, [], [])
  | Const_string _ -> (B.uString, [], [])
  | _ -> assert false

and replace_params ps fs =
  List.map2 (fun (i, _, v) f -> (i, f, v)) ps fs

and get_unint_cstrref env name args =
  let ucf = Le.find (Path.Pident (Ident.create_persistent name)) env in
  let f   = match ucf with F.Farrow _ -> F.apply ucf args | _ -> ucf in
    F.get_refinement f

and get_cstrrefs env path tag name args shp =
  let preds  = List.map expression_to_pexpr args in
  let mref   = try F.const_refinement 
                [M.assert_constructed_expr env preds (path, tag) shp] with Not_found -> [] in  
  let tagref = B.tag_refinement path tag in
  let lhsref = F.empty_refinement in
  let rhsref = mref @ tagref in
  let rhsref = match get_unint_cstrref env name (List.map expression_to_pexpr args) with Some ucr -> ucr @ rhsref | None -> rhsref in
  let wfref  = lhsref in
    (lhsref, rhsref, wfref) 

and constrain_constructed (env, guard, f) cstrdesc args e =
  let shp = F.fresh_false e.exp_env e.exp_type in
  match F.unfold shp with
  | F.Fsum (path, ro, cstrs, _) ->
      let tag                     = cstrdesc.cstr_tag in
      let (name, _)               = List.assoc tag cstrs in
      let (lhsref, rhsref, wfref) = get_cstrrefs env path tag name args shp in
      let (argframes, argcs)      = constrain_subexprs env guard args in
      let cstrs                   = List.map (fun (t, (n, ps)) -> (t, (n, if t = tag then replace_params ps argframes else ps))) cstrs in
      let f'                      = F.Fsum (path, ro, cstrs, lhsref) in
        (constrain_fold (env, guard, f) f' rhsref wfref [] argcs)
  | _ -> assert false

and constrain_fold (env, guard, f) f'' rhsref wfref cstrs subcstrs =
  let f' = F.unfold_applying f in
    (F.append_refinement rhsref f, WFFrame (env, (F.append_refinement wfref f)) :: SubFrame (env, guard, f'', f') :: cstrs, subcstrs)

and compare_labels ({lbl_pos = n}, _) ({lbl_pos = m}, _) =
  compare n m

and constrain_record (env, guard, f) labeled_exprs =
  let (_, sorted_exprs)                = List.split (List.sort compare_labels labeled_exprs) in
  let (p, ps)                          = match f with F.Fsum(p, _, [(_, (_, ps))], _) -> (p, ps) | _ -> assert false in
  let (fs, subexp_cs)                  = constrain_subexprs env guard sorted_exprs in
  let to_field (id, _, v) f            = (id, f, v) in
  let field_qualifier (id, _, _) fexpr = B.field_eq_qualifier id (expression_to_pexpr fexpr) in
  let params                           = List.map2 to_field ps fs in
  let field_refs                       = F.mk_refinement [] (List.map2 field_qualifier ps sorted_exprs) [] in
  let f'                               = F.record_of_params p params field_refs in
    (constrain_fold (env, guard, f) f' F.empty_refinement F.empty_refinement [] subexp_cs)

and constrain_field (env, guard, _) expr label_desc =
  let (recframe, cstrs)       = constrain expr env guard in
  let (fieldname, fieldframe) = match F.unfold_applying recframe with
    | F.Fsum (_, _, [(_, (_, ps))], _) -> let (i, f, _) = List.nth ps label_desc.lbl_pos in (i, f)
    | _ -> assert false
  in (fieldframe, [WFFrame (env, fieldframe)], cstrs)

and constrain_setfield (env, guard, f) expr label_desc expr' =
  let _ = assert false in
  (B.uUnit, [], [])

and constrain_if (env, guard, f) e1 e2 e3 =
  let (f1, cstrs1) = constrain e1 env guard in
  let guardvar     = Path.mk_ident "guard" in
  let env'         = Le.add guardvar f1 env in
  let guard2       = (guardvar, true)::guard in
  let (f2, cstrs2) = constrain e2 env' guard2 in
  let guard3       = (guardvar, false)::guard in
  let (f3, cstrs3) = constrain e3 env' guard3 in
    (f,
    [WFFrame(env, f); SubFrame(env', guard2, f2, f); SubFrame(env', guard3, f3, f)],
    cstrs1 @ cstrs2 @ cstrs3)

and bind env guard p f pexpr =
  log_frame p.pat_loc f;
  F.env_bind env p.pat_desc f

and mk_constructor_pat_guard tenv env (_, path, tag, args) =
  try
    let args  = List.map (function Some v -> P.Var v | None -> raise Not_found) args in
    let cstrs = Env.constructors_of_type path (Env.find_type path tenv) in
    let cname = fst (List.find (fun (n, c) -> c.cstr_tag = tag) cstrs) in
      get_unint_cstrref env cname args
  with Not_found ->
    None

and add_constructor_pattern tenv env ((v, _, _, _) as cpat) =
  match mk_constructor_pat_guard tenv env cpat with
    | None   -> env
    | Some r -> Le.add v (F.append_refinement r (Le.find v env)) env

and mk_match_guarded_env env pat = function
  | P.Var v ->
      let cps = Pattern.constructor_patterns v pat in
      let env = List.fold_left (add_constructor_pattern pat.pat_env) env cps in
        Le.add (Path.mk_ident "__measure_guard") (Builtins.rUnit "" (Path.mk_ident "") (M.mk_guard env v cps)) env
  | _ -> env

and constrain_case (env, guard, f) matchf matche (pat, e) =
  let env         = bind env guard pat matchf matche in
  let env         = mk_match_guarded_env env pat matche in
  let (fe, subcs) = constrain e env guard in
    (SubFrame (env, guard, fe, f), subcs)

and constrain_match (env, guard, f) e pexps partial =
  let (matchf, matchcstrs) = constrain e env guard in
  let cases                = List.map (constrain_case (env, guard, f) matchf (expression_to_pexpr e)) pexps in
  let (cstrs, subcstrs)    = List.split cases in
    (f, WFFrame (env, f) :: cstrs, List.concat (matchcstrs :: subcstrs))

and constrain_function (env, guard, f) pat e' =
  match f with
    | F.Farrow (_, f, unlabelled_f') ->
        let _ = F.refinement_iter 
          (fun r -> Constraint.formals_addn (F.refinement_qvars r)) f in
        let env' = F.env_bind env pat.pat_desc f in
        begin match e'.exp_desc with
          | Texp_function ([(pat', e')], _) ->
              let (f', cs, lcs) = constrain_function (env', guard, unlabelled_f') pat' e' in
              let f             = F.Farrow (pat.pat_desc, f, f') in
                (f, WFFrame (env, f) :: cs, lcs)
          | _ ->
              let (f'', cstrs) = constrain e' env' guard in
              let f'           = F.label_like unlabelled_f' f'' in
              let f            = F.Farrow (pat.pat_desc, f, f') in
                (f, [WFFrame (env, f); SubFrame (env', guard, f'', f')], cstrs)
        end
    | _ -> assert false

and instantiate_id id f env tenv =
  let env_f =
    try
      Le.find id env
    with Not_found ->
      Frame.fresh_without_vars tenv ((Env.find_value id tenv).val_type)
  in
    F.instantiate env env_f f

and constrain_base_identifier (env, _, f) id e =
  let refn =
    if Le.mem id env then B.equality_refinement (expression_to_pexpr e) else F.empty_refinement in
  let f' = instantiate_id id f env e.exp_env in
  let f  = F.label_like f f' in
    (F.apply_refinement refn f', [WFFrame (env, f)], [])

and constrain_identifier (env, _, f) id tenv =
  let f' = instantiate_id id f env tenv in
  let f  = F.label_like f f' in
    (f', [WFFrame(env, f)], [])

and apply_once env guard (f, cstrs, subexp_cstrs) e = match (f, e) with
  | (F.Farrow (p, f1, _), (Some arg, _)) ->
      let (farg, arg_cstrs) = constrain arg env guard in
      let f2                = F.apply f [expression_to_pexpr arg] in
        (f2, SubFrame (env, guard, farg, f1) :: cstrs, arg_cstrs @ subexp_cstrs)
  | _ -> assert false

and constrain_application (env, guard, _) func exps =
  let (func_frame, func_cstrs) = constrain func env guard in
    List.fold_left (apply_once env guard) (func_frame, [], func_cstrs) exps

and constrain_let (env, guard, f) recflag bindings body =
  let (env', cstrs1)       = constrain_bindings env guard recflag bindings in
  let (body_frame, cstrs2) = constrain body env' guard in
  match body.exp_desc with
    | Texp_let _ | Texp_function _ | Texp_ifthenelse _ | Texp_match _ ->
        (body_frame, [WFFrame (env, body_frame)], cstrs1 @ cstrs2)
    | _ ->
      let f = F.label_like f body_frame in
        (f, [WFFrame (env, f); SubFrame (env', guard, body_frame, f)], cstrs1 @ cstrs2)

and constrain_array (env, guard, f) tenv elements =
  let (f, fa) =
    match f with
      | F.Fabstract(p, ([(_, fa, _)] as ps), id, _) ->
          (F.Fabstract(p, ps, id, B.size_lit_refinement (List.length elements) tenv), fa)
      | _ -> assert false in
  let list_rec (fs, c) e = (fun (f, cs) -> (f::fs, cs @ c)) (constrain e env guard) in
  let (fs', sub_cs)      = List.fold_left list_rec ([], []) elements in
  let mksub b a          = SubFrame(env, guard, a, b) in
    (f, WFFrame(env, f) :: List.map (mksub fa) fs', sub_cs)

and constrain_sequence (env, guard, f) e1 e2 =
  let (f1, cs1) = constrain e1 env guard in
  let env'      = Le.add (Path.mk_ident "seq") f1 env in
  let (f', cs2) = constrain e2 env' guard in
    (f, [WFFrame (env, f); SubFrame (env', guard, f', f)], cs1 @ cs2)

and elem_qualifier fexpr n =
  B.proj_eq_qualifier n (expression_to_pexpr fexpr)

and constrain_tuple (env, guard, f) es =
  let (fs, subexp_cs) = constrain_subexprs env guard es in
  let f'              = F.tuple_of_frames fs (F.mk_refinement [] (Miscutil.mapi elem_qualifier es) []) in
    (* We can't just use f' directly because we might need substitutions to make the new
       tuple self-contained (e.g., to refer to e0 instead of some local var) *)
    (f, [WFFrame(env, f); SubFrame (env, guard, f', f)], subexp_cs)

and constrain_assertfalse (env, _, _) tenv ty =
  let f = F.fresh_false tenv ty in
    (f, [WFFrame (env, f)], [])

and constrain_guard (env, guard, f) form e =
  let (af, cstrs) = constrain e env guard in
  let testvar     = Path.mk_ident "test_predicate" in
  let env'        = Le.add testvar af env in
  let witness     = B.mk_unit [(Path.mk_ident "", Path.mk_ident "", P.Boolexp (P.Var testvar))] in
    (f,
      (WFFrame (env', f) :: SubFrame (env', guard, witness, f) ::
         match form with
           | Texp_assume _ -> []
           | Texp_assert _ -> [SubFrame (env', guard, B.uUnit, witness)]
           | _             -> assert false),
     cstrs)

and constrain_and_bind guard (env, cstrs) (pat, e) =
  let (f, cstrs') = F.begin_def (); let r = constrain e env guard in F.end_def (); r in
  let env         = bind env guard pat (F.generalize f) (expression_to_pexpr e) in
    (env, cstrs @ cstrs')

and bind_all bindings fs tenv env guard =
  List.fold_right2 (fun (p, e, px) f env -> bind env guard p f px) bindings fs env

and constrain_bindings env guard recflag bindings =
  match recflag with
  | Default | Nonrecursive -> List.fold_left (constrain_and_bind guard) (env, []) bindings
  | Recursive ->
    let tenv       = (snd (List.hd bindings)).exp_env in
    let (_, exprs) = List.split bindings in
    let bindings   = List.map (fun (p, e) -> (p, e, expression_to_pexpr e)) bindings in

    (* We need to figure out all the frames' labels before we can properly bind them. *)
    let _                 = F.begin_def () in
    let unlabeled_frames  = List.map (fun e -> F.initialize_type_expr_levels e.exp_type; F.fresh e.exp_env e.exp_type) exprs in
    let unlabeled_env     = bind_all bindings unlabeled_frames tenv env guard in
    let (label_frames, _) = constrain_subexprs unlabeled_env guard exprs in
    let _                 = F.end_def () in

    let binding_frames = List.map2 F.label_like unlabeled_frames label_frames in
    let binding_frames = List.map F.generalize binding_frames in
            
    (* Redo constraints now that we know what the right labels are *)
    let bound_env                    = bind_all bindings binding_frames tenv env guard in
    let _                            = F.begin_def () in
    let (found_frames, subexp_cstrs) = constrain_subexprs bound_env guard exprs in
    let _                            = F.end_def () in
    let found_frames                 = List.map F.generalize found_frames in

    let make_cstr fc = {lc_cstr = fc; lc_tenv = tenv; lc_orig = Loc Location.none; lc_id = fresh_fc_id ()} in
    let build_found_frame_cstr_list cs found_frame binding_frame =
      make_cstr (WFFrame (bound_env, binding_frame)) ::
      make_cstr (SubFrame (bound_env, guard, found_frame, binding_frame)) :: cs
    in (bound_env, (List.fold_left2 build_found_frame_cstr_list [] found_frames binding_frames) @ subexp_cstrs)

and constrain_subexprs env guard es =
  List.fold_right (fun e (fs, cs) -> let (f, cs') = constrain e env guard in (f :: fs, cs' @ cs)) es ([], [])

let constrain_structure tenv initfenv initquals str =
  let rec constrain_rec quals fenv cstrs = function
    | [] -> (quals, fenv, cstrs)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs') = constrain exp fenv [] in
          constrain_rec quals fenv (cstrs' @ cstrs) srem
    | (Tstr_value (recflag, bindings))::srem ->
        let (fenv, cstrs') = constrain_bindings fenv [] recflag bindings in
          constrain_rec quals fenv (cstrs @ cstrs') srem
    | (Tstr_type _)::srem ->
        constrain_rec quals fenv cstrs srem
    | _ -> assert false
  in constrain_rec initquals initfenv [] str

(******************************************************************************)
(***************************** Qualifying modules *****************************)
(******************************************************************************)

let pre_solve sourcefile =
  C.cprintf C.ol_solve_master "@[##solve %s##@\n@]" sourcefile; Bstats.reset ()

let post_solve () = 
  if C.ck_olev C.ol_timing then
    (Printf.printf "##time##\n"; Bstats.print stdout "\nTime to solve constraints:\n";
    Printf.printf "##endtime##\n"; (*TheoremProver.dump_simple_stats ()*))

let lbl_dummy_cstr env c =
  { lc_cstr = c; lc_tenv = env; lc_orig = Loc (Location.none); lc_id = fresh_fc_id () }

let maybe_cstr_from_unlabel_frame fenv p f = 
  if Le.mem p fenv
  then
    let f' = Le.find p fenv in
    try Some (SubFrame (fenv, [], f', F.label_like f f'))
      with Failure s -> printf "@[Failure@ %s:@ %a@ <>@ %a@]@." s F.pprint f F.pprint f'; assert false
  else
    None 

let nrframes = 
  let n = ref [] in
  ((fun i -> n := i :: !n), (fun () -> !n)) 
let add_nrframe = fst nrframes 
let get_nrcstrs env fenv = List.rev_map (fun f -> lbl_dummy_cstr env (WFFrame (fenv, f))) ((snd nrframes) ())

let warn_invalid_mlq env p f = 
  let s = (fun _ -> []) in     
  let env = Le.add C.qual_test_var f env in
  let aa = P.Var C.qual_test_var in
  F.refinement_iter (fun r -> if not (WF.refinement_well_formed env s r aa) then
          eprintf "@[Warning:@ %s@ ::@ [...]@ %a@ [...]@ may@ not@ be@ wf@]@." (Path.name p) F.pprint_refinement r) f
     
let qualify_implementation sourcefile fenv' ifenv env qs consts str =
  let _              = if !Clflags.ck_mlq then Le.iter (warn_invalid_mlq fenv') ifenv in
  let _              = reset_framelog () in
  let (qs, fenv, cs) = constrain_structure env fenv' qs str in
  let nrcs           = get_nrcstrs env fenv in
  let cs             = List.rev_append nrcs cs in
  let cs             = (List.map (lbl_dummy_cstr env) (Le.maplistfilter (maybe_cstr_from_unlabel_frame fenv) ifenv)) @ cs in
  let _              = pre_solve sourcefile in
  (*let (s,cs)         = Bstats.time "solving" (solve qs env consts) cs in*)
  let solver         = if !Clflags.use_fixpoint then Fixsolve.solver else dsolver in
  let (s, cs)        = solve_with_solver qs env consts cs solver in
  let _              = post_solve () in
  let flog           = if !Clflags.raw_frames then !flog else framemap_apply_solution s !flog in
  let _              = dump_frames sourcefile flog in
    match cs with [] -> () | _ ->
      (Printf.printf "Errors encountered during type checking:\n\n";
       flush stdout; raise (Errors(List.map (make_frame_error s) cs)))
