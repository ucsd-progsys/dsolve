(* -*- tab-width: 2; -*- *)

open Asttypes
open Typedtree
open Btype
open Types
open Constraint
open Longident
open Location
open Format

module P = Predicate
module C = Common
module Qg = Qualgen
module Cf = Clflags
module B = Builtins
module Le = Lightenv
module F = Frame

type error =
  | NotSubtype of F.t * F.t
  | IllFormed of F.t
  | AssertMayFail

exception Error of Location.t * error
exception Errors of (Location.t * error) list

module LocationMap = Map.Make(struct type t = Location.t
                                     let compare = compare end)
let expression_to_pexpr e =
  match e.exp_desc with
  | Texp_constant (Const_int n) -> P.PInt n
  | Texp_ident (id, _) -> P.Var id
  | _ -> P.Var (Path.mk_ident "dummy")

let get_true_tag env =
  match (Env.lookup_constructor (Lident "true") env).cstr_tag with
  |  Cstr_constant n -> n
  | _ -> assert false

let under_lambda = ref 0

let rec constrain e env guard cstrs framemap =
  let constrain_subexprs subenv exprs cs fm = 
    List.fold_right (subexpr_folder subenv guard) exprs ([], cs, fm) in
  let (f, cs, fm) =
    match (e.exp_desc,repr e.exp_type) with
    (Texp_constant(Const_int n), {desc = Tconstr(path, [], _)}) ->
      let _ = if !Cf.dump_qualifs then Qg.add_constant n else 5 in
      (F.Fconstr (path, [], B.equality_refinement (P.PInt n)),
           cstrs, framemap)
  | (Texp_constant(Const_float n), t) ->
    (F.fresh_without_vars e, cstrs, framemap)
  | (Texp_construct(cstrdesc, []), {desc = Tconstr(path, [], _)}) ->
          let cstrref =
            match cstrdesc.cstr_tag with
                Cstr_constant n ->
                  B.equality_refinement (P.PInt n)
              | _ -> F.empty_refinement
          in (F.Fconstr (path, [], cstrref), cstrs, framemap)
  | (Texp_record (labeled_exprs, None), {desc = (Tconstr _)}) ->
    let compare_labels ({lbl_pos = n}, _) ({lbl_pos = m}, _) = compare n m in
    let (_, sorted_exprs) = List.split (List.sort compare_labels labeled_exprs) in
    let (subframes, new_cs, new_fm) = constrain_subexprs env sorted_exprs cstrs framemap in
    let subframe_field cs_rest fsub (fsup, _, _) = 
      SubFrame (env, guard, fsub, fsup, Loc e.exp_loc, fresh_fc_id ()) :: cs_rest in
    let f = F.fresh e in
    let new_cs = match f with
      | F.Frecord (_, recframes, _) ->
          List.fold_left2 subframe_field new_cs subframes recframes
      | _ -> assert false
    in
    let f = match f with
      | F.Frecord (p, recframes, _) ->
          let field_qualifier (_, name, _) fexpr = B.field_eq_qualifier name (expression_to_pexpr fexpr) in
            F.Frecord (p, recframes, ([], F.Qconst (List.map2 field_qualifier recframes sorted_exprs)))
      | _ -> assert false
    in (f, WFFrame (env, f, Loc e.exp_loc,fresh_fc_id()) :: new_cs, new_fm)
  | (Texp_field (expr, label_desc), t) ->
    let (recframe, new_cs, new_fm) = constrain expr env guard cstrs framemap in
    let (fieldframe, fieldname) = match recframe with
      | F.Frecord (_, fs, _) ->
          let rf = List.nth fs label_desc.lbl_pos in
          begin match rf with
              (fr, name, _) -> (fr, name)
          end
      | _ -> assert false
    in
    let pexpr = P.Field (fieldname, expression_to_pexpr expr) in
    let f = F.apply_refinement (B.equality_refinement pexpr) fieldframe in
      (f, WFFrame (env, f, Loc e.exp_loc,fresh_fc_id()) :: new_cs, new_fm)
  | (Texp_ifthenelse(e1, e2, Some e3), t) ->
          let f = F.fresh e in
          let (f1, cstrs1, fm1) = constrain e1 env guard cstrs framemap in
          let guardvar = Path.mk_ident "guard" in
          let true_tag = get_true_tag e.exp_env in
          let guardp = P.equals (P.Var guardvar, P.PInt true_tag) in
          let env' = Le.add guardvar f1 env in
          let guard2 = P.And (guardp, guard) in
          let (f2, cstrs2, fm2) = constrain e2 env' guard2 cstrs1 fm1 in
          let guard3 = P.And (P.Not guardp, guard) in
          let (f3, cstrs3, fm3) = constrain e3 env' guard3 cstrs2 fm2 in
            (f,
             WFFrame(env, f, Loc e.exp_loc,fresh_fc_id())::
             SubFrame(env', guard2, f2, f, Loc e.exp_loc,fresh_fc_id())::
             SubFrame(env', guard3, f3, f, Loc e.exp_loc,fresh_fc_id())::cstrs3,
             fm3)
  | (Texp_function([(pat, e')], _), t) ->
    begin match F.fresh e with
      | F.Farrow (_, f, unlabelled_f') ->
          let _ =
            match (t.desc, pat.pat_desc) with
                (* pmr: needs to be generalized to match other patterns *)
                (Tarrow(_, t_in, t_out, _), Tpat_var x) ->
                  if !Cf.dump_qualifs then Qg.add_label(Path.Pident x, t_in) else ()
              | _ -> ()
          in
            (*match e'.exp_type with  
              {desc = Tarrow(_, t_in, _, _)} -> Printf.printf "%s%s\n" (Ident.name x) (F.type_structure t_in); Qg.add_label (xp, t_in)
              | {desc = d} -> Printf.printf "%s%s\n" (Ident.name x) (F.type_structure e'.exp_type); Qg.add_label (xp, e'.exp_type)*)
          let env' = Pattern.env_bind env pat.pat_desc f in
          let (f'', cstrs', fm') = constrain e' env' guard cstrs framemap in
            (* Since the underlying type system doesn't have dependent
               types, fresh can't give us the proper labels for the RHS.
               Instead, we have to label it after the fact. *)
          let f' = F.label_like unlabelled_f' f'' in
          let f = F.Farrow (Some pat.pat_desc, f, f') in
            (f,
             WFFrame (env, f, Loc e.exp_loc,fresh_fc_id())
             :: SubFrame (env', guard, f'', f', Loc e.exp_loc,fresh_fc_id())
             :: cstrs',
             fm')
      | _ -> assert false
    end 
  | (Texp_ident (id, _), {desc = Tconstr (p, [], _)} ) ->
    let r = B.equality_refinement (expression_to_pexpr e) in
    (* Tconstr could be a record or frame to us, so we punt the issue *)
    let f = Le.find id env in
      (F.apply_refinement r f, cstrs, framemap)
(* ming: subtyping may be better for the inner types when a complex type is
 * pulled up by name, but we have no way of expressing what we actually want,
 * which is that each element is of the same shape with an equality_refinement
 * *)
(*| (Texp_ident _, {desc = Tconstr (p, l, _)}) ->
          (F.Fconstr (p, l, B.equality_refinement (expression_to_pexpr e)),
           cstrs, framemap)*)
  | (Texp_ident (id, _), t) ->
          let (f', ftemplate) = (
              (try Le.find id env
                with Not_found -> fprintf std_formatter "@[Not_found:@ %s@]" (Path.unique_name id);
                     raise Not_found)
                , F.fresh e) in
          (*let _ = fprintf std_formatter "@[instantiating@ %a@ %a@]" F.pprint f' F.pprint ftemplate in*)
          (*let _ = F.pprint Format.err_formatter f' in
          let _ = F.pprint Format.err_formatter ftemplate in *)
          let f = F.instantiate f' ftemplate in
            (f, WFFrame(env, f, Loc e.exp_loc,fresh_fc_id())::cstrs, framemap)
  | (Texp_apply (e1, exps), _) ->
    let constrain_application (f, cs, fm) = function
        | (Some e2, _) ->
          begin match f with
	          | F.Farrow (l, f, f') ->
	            let (f2, cs', fm') = constrain e2 env guard cs fm in
	            let f'' = match l with
                  | Some pat ->
                    (* We _must_ apply a substitution over the whole pattern or
                       else we risk capturing stuff in the environment (e.g,
                       apply (a, b) -> v > a in the context where a is defined).
                       This risks severe unsoundness. *)
                    let subs = Pattern.bind_pexpr pat (expression_to_pexpr e2) in
                      List.fold_right F.apply_substitution subs f'
                      (* pmr: The soundness of this next line is suspect,
                         must investigate (i.e., what if there's a var that might
                         somehow be substituted that isn't because of this?  How
                         does it interact w/ the None label rules for subtyping? *)
                  | _ -> f'
              in
              (f'', SubFrame (env, guard, f2, f, Loc e2.exp_loc,fresh_fc_id()) :: cs', fm')
	          | _ -> assert false
          end
        | _ -> assert false
        in List.fold_left constrain_application
               (constrain e1 env guard cstrs framemap) exps
  | (Texp_let (recflag, bindings, body_exp), t) ->
    let (env, cstrs, fmap) = constrain_bindings env guard cstrs framemap recflag bindings in 
    let (body_frame, cstrs, fmap) = constrain body_exp env guard cstrs fmap in
    let f = F.fresh_with_labels e body_frame in
      (f,
       WFFrame (env, f, Loc e.exp_loc,fresh_fc_id())
       :: SubFrame (env, guard, body_frame, f, Loc body_exp.exp_loc, fresh_fc_id())
       :: cstrs,
       fmap)
  | (Texp_array(es), t) ->
          let _ = if !Cf.dump_qualifs then Qg.add_constant (List.length es) else 5 in
          let f = F.fresh e in
          let (f, fs) = 
            (function 
              | F.Fconstr(p, l, _) -> (F.Fconstr(p, l, B.size_lit_refinement(List.length es)), l) 
              | _ -> assert false) f in
          let list_rec (fs, c, m) e = 
            (function (f, c, m) -> (f::fs, c, m)) (constrain e env guard c m) in
          let (fs', c, m) = List.fold_left list_rec ([], cstrs, framemap) es in
          let mksub b a = SubFrame(env, guard, a, b, Loc e.exp_loc, fresh_fc_id()) in
          let c = List.append (List.map (mksub (List.hd fs)) fs') c in
          (f, WFFrame(env, f, Loc e.exp_loc,fresh_fc_id())::c, m)
  | (Texp_sequence(e1, e2), t) ->
          let (f1, c, m) = constrain e1 env guard cstrs framemap in
          let (f2, c, m) = constrain e2 env guard c m in
           (* ming: the ocaml typer isn't constraining sequenced expressions
            * into statements, so i guess we can't either *)
          (f2, ((*SubFrame(env, guard, f1, B.mk_unit ())::*)c), m)
  | (Texp_tuple(es), t) ->
    let constrain_exprs e (fs, (_, c, m)) =
      let (f, new_c, new_m) = constrain e env guard c m in
        (f :: fs, (f, new_c, new_m))
    in
    (* We use Funknown here because it's never going to get looked at anyway *)
    let (fs, (_, new_cs, new_m)) = List.fold_right constrain_exprs es ([], (F.Funknown, cstrs, framemap)) in
    let f = F.fresh e in
      begin match f with
        | F.Ftuple fresh_fs ->
            let new_cs = List.fold_left2
              (fun cs rec_frame fresh_frame -> 
                SubFrame (env, guard, rec_frame, fresh_frame, Loc e.exp_loc, fresh_fc_id()) :: cs)
              new_cs fs fresh_fs in
            (f, (*(env, f) ::*) new_cs, new_m)
        | _ -> assert false
      end
  | (Texp_assertfalse, t) ->
    (F.fresh e, cstrs, framemap)
  | (Texp_assert e', t) ->
    let (f, cstrs, fm) = constrain e' env guard cstrs framemap in
    let guardvar = Path.mk_ident "assert_guard" in
    let env = Le.add guardvar f env in
    let true_tag = get_true_tag e.exp_env in
    let assert_qualifier =
      (Path.mk_ident "assertion",
       Path.mk_ident "null",
       P.equals (P.Var guardvar, P.PInt true_tag))
    in (B.mk_unit (),
        SubFrame (env, guard, B.mk_int [], B.mk_int [assert_qualifier], Assert e.exp_loc, fresh_fc_id()) :: cstrs,
        fm)
  | (_, t) ->
    (* As it turns out, giving up and returning true here is actually _very_ unsound!  We won't check subexpressions! *)
    fprintf err_formatter "@[Warning: Don't know how to constrain expression, structure:@ %a@ location:@ %a@]@.@." Printtyp.raw_type_expr t Location.print e.exp_loc; flush stderr;
    assert false
  in (f, cs, LocationMap.add e.exp_loc f fm)

and constrain_bindings env guard cstrs framemap recflag bindings =
  match recflag with
  | Default
  | Nonrecursive ->
    let constrain_and_bind (env, cstrs, fm) (pat, e) =
      let _ = if !under_lambda = 0 || not(!Cf.less_qualifs) then
        match pat.pat_desc with
        | Tpat_var x -> if !Cf.dump_qualifs then Qg.add_label (Path.Pident x, e.exp_type) else ()
        (*| Tpat_tuple *)
        | _ -> ()
      else () in
      let lambda = match e.exp_desc with
      | Texp_function (_, _) -> 
        under_lambda := !under_lambda + 1; true 
      | _ -> false in
      let (f, cstrs, fm) = constrain e env guard cstrs fm in
      let env = Pattern.env_bind env pat.pat_desc f in
      let _ = if lambda then under_lambda := !under_lambda - 1 else () in
        (env, cstrs, fm)
    in
      (* Since these are nonrecursive, I don't think it matters what order
         we bind them in.  We might as well do it the fast way. *)
      List.fold_left constrain_and_bind (env, cstrs, framemap) bindings
	| Recursive ->
    (* This is horrendous, but about the best we can do
       without using destructive updates.  We need to label
       the function we're binding (if any) in the environment where we
       also constrain the function.  Unfortunately, we don't
       have that label until after we constrain it!  So we do
       a first pass just to get the labels, then do a second
       with the proper labels added. *)
    let (vars, exprs) = List.split bindings in

    (* Determine the labels we need to have on our bound frames first *)
    let no_label_frame e = F.fresh e in
    let unlabeled_frames = List.map no_label_frame exprs in
    let binding_var = function
    | {pat_desc = Tpat_var f} -> Path.Pident f
    | _ -> assert false
    in
    let vars = List.map binding_var vars in
    let unlabeled_env = Le.addn (List.combine vars unlabeled_frames) env in
    let labeling_constraints = List.map (fun e -> constrain e unlabeled_env guard cstrs framemap) exprs in
    let (label_frames, _, _) = Misc.split3 labeling_constraints in
    (* pmr: I'm not assuming that constrain always gives us a fresh frame here, otherwise we could
     use label_frames directly *)
    let binding_frames = List.map2 F.label_like unlabeled_frames label_frames in
            
    (* ming: qualgen code. generate qualifiers for all binding vars if
     * we're currently under a lambda build a bitmap for each bind to
     * manage lambda detection *)
    let qualgen_addlbls var exp = if !Cf.dump_qualifs then Qg.add_label (var, exp.exp_type) else () in
    let _ = if !under_lambda = 0 || not(!Cf.less_qualifs) then List.iter2 qualgen_addlbls vars exprs  
            else () 
    in
    let qualgen_is_function e = 
    match e.exp_desc with
    | Texp_function (_, _) -> true
    | _ -> false 
    in
    let qualgen_incr_lambda () = under_lambda := !under_lambda + 1 in
    let qualgen_decr_lambda () = under_lambda := !under_lambda - 1 in

    (* Redo constraints now that we know what the right labels are --- note that unlabeled_frames are all
     still essentially fresh, since we're discarding any constraints on them *)
    let bound_env = Le.addn (List.combine vars binding_frames) env in
    (* qualgen, continued.. wrap the fold function in another that
     * pushes onto the lambda stack while constraining functions *)
    let qualgen_wrap_found_frame_list e b = 
      if qualgen_is_function e then 
        let _ = qualgen_incr_lambda () in
        let r = subexpr_folder bound_env guard e b in
        let _ = qualgen_decr_lambda () in
        r  
      else subexpr_folder bound_env guard e b
    in
                  
    let (found_frames, cstrs, fmap) = List.fold_right qualgen_wrap_found_frame_list exprs ([], cstrs, framemap) in
    (* Ensure that the types we discovered for each binding are no more general than the types implied by
     their uses *)
    (* pmr: This is going to take some work to resolve; for now, default on the locations because we don't
       have anything that'll butt up against it. *)
    let build_found_frame_cstr_list cs found_frame binding_frame =
      WFFrame (bound_env, binding_frame, Loc Location.none,fresh_fc_id()) :: 
      SubFrame (bound_env, guard, found_frame, binding_frame, Loc Location.none,fresh_fc_id()) :: cs
    in
    let cstrs = List.fold_left2 build_found_frame_cstr_list cstrs found_frames binding_frames in
    (bound_env,
     cstrs,
     fmap)

and subexpr_folder subenv guard e (fframes, cs, fm) =
  let (frame, new_cs, new_fm) = constrain e subenv guard cs fm in
    (frame :: fframes, new_cs, new_fm)

let constrain_structure initfenv initquals str =
  let rec constrain_rec quals fenv cstrs fmap = function
    | [] -> (quals, fenv, cstrs, fmap)
    | (Tstr_eval exp) :: srem ->
        let (_, cstrs', fmap') =
          constrain exp fenv P.True cstrs fmap
        in constrain_rec quals fenv cstrs' fmap' srem
    | (Tstr_qualifier (name, (valu, pred))) :: srem ->
        let quals = (Path.Pident name, Path.Pident valu, pred) :: quals in
          constrain_rec quals fenv cstrs fmap srem
		| (Tstr_value (recflag, bindings))::srem ->
        let (fenv, cstrs, fmap) =
          constrain_bindings fenv P.True cstrs fmap recflag bindings
        in constrain_rec quals fenv cstrs fmap srem
    | (Tstr_type(_))::srem ->
        (*Printf.printf "Ignoring type decl";*) constrain_rec quals fenv cstrs fmap srem
    | _ -> assert false
  in constrain_rec initquals initfenv [] LocationMap.empty str

module QualifierSet = Set.Make(Qualifier)

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from the environment. *)
let instantiate_in_environments cs qs =
  let envs = List.map (function SubFrame (e,_,_,_,_,_) | WFFrame (e,_,_,_) -> e) cs in
  let instantiate_qual qualset q =
    let instantiate_in_env qset env =
      try
        QualifierSet.add (Qualifier.instantiate env q) qset
      with Qualifier.Refinement_not_closed -> qset
    in List.fold_left instantiate_in_env qualset envs
  in QualifierSet.elements (List.fold_left instantiate_qual QualifierSet.empty qs)

let framemap_apply_solution s fmap = 
  LocationMap.map (F.apply_solution s) fmap

let dump_frames sourcefile fmap =
  let dump_frame pp loc fr =
    if loc = Location.none then () else begin
    Stypes.print_position pp loc.loc_start;
    fprintf pp " ";
    Stypes.print_position pp loc.loc_end;
    fprintf pp "@.type(@.  ";
    F.pprint pp fr;
    fprintf pp "@.)@."
    end
  in
  if !Cf.dump_frames then
    let filename = Misc.chop_extension_if_any sourcefile ^ ".annot" in
    let pp = formatter_of_out_channel (open_out filename) in
      LocationMap.iter (dump_frame pp) fmap

let make_frame_error s cstr =
  let rec error_rec cstr =
    let orig = match cstr with
      | SubFrame (_, _, _, _, o,_)
      | WFFrame (_, _, o,_) -> o
    in
    match orig with
      | Loc loc ->
        begin match cstr with
          | SubFrame (_, _, f1, f2, _,_) ->
            (loc, NotSubtype (F.apply_solution s f1,  F.apply_solution s f2))
          | WFFrame (_,f,_,_) -> (loc, IllFormed (F.apply_solution s f))
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

let pre_solve () = 
  C.cprintf C.ol_solve_master "@[##solve##@\n@]"; Bstats.reset ()

let post_solve () = 
  if C.ck_olev C.ol_timing then
    (Printf.printf "##time##\n"; Bstats.print stdout "\nTime to solve constraints:\n";
    Printf.printf "##endtime##\n"; TheoremProver.dump_simple_stats ())

let qualify_implementation sourcefile fenv qs str =
  let (qs, _, cs, fmap) = constrain_structure fenv qs str in
  let inst_qs = instantiate_in_environments cs qs in
  if List.length cs = 0 then LocationMap.empty else
    let _ = pre_solve () in
    let (s,cs) = Bstats.time "solving" (solve inst_qs) cs in
    let _ = post_solve () in
    let _ = dump_frames sourcefile (framemap_apply_solution s fmap) in
    match cs with [] -> fmap | _ ->
      (Printf.printf "Errors encountered during type checking:\n\n";
      flush stdout; raise (Errors(List.map (make_frame_error s) cs)))

let qualgen_nasty_hack fenv str =
  ignore (constrain_structure fenv [] str)
