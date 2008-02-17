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

let expression_to_pexpr e =
  match e.exp_desc with
    | Texp_constant (Const_int n) -> P.PInt n
    | Texp_ident (id, _) -> P.Var id
    | _ -> P.Var (Path.mk_ident "dummy")

let under_lambda = ref 0

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

let label_constraint exp fc =
  let org = match exp.exp_desc with Texp_assert _ -> Assert exp.exp_loc | _ -> Loc exp.exp_loc in
    {lc_cstr = fc; lc_orig = org; lc_id = fresh_fc_id()}

let rec constrain e env guard =
  let environment = (env, guard, Frame.fresh e.exp_env e.exp_type) in
  let (f, cstrs, rec_cstrs) =
    match (e.exp_desc, repr e.exp_type) with
      | (Texp_constant const_typ, {desc = Tconstr(path, [], _)}) -> constrain_constant path const_typ
      | (Texp_construct (cstrdesc, args), {desc = Tconstr(_, _, _)}) -> constrain_constructed environment e.exp_env cstrdesc args
      | (Texp_record (labeled_exprs, None), {desc = (Tconstr _)}) -> constrain_record environment labeled_exprs
      | (Texp_field (expr, label_desc), _) -> constrain_field environment expr label_desc
      | (Texp_ifthenelse (e1, e2, Some e3), _) -> constrain_if environment e1 e2 e3
      | (Texp_function ([(pat, e')], _), t) -> constrain_function environment t pat e'
      | (Texp_ident (id, _), {desc = Tconstr (p, [], _)} ) -> constrain_base_identifier env id e
      | (Texp_ident (id, _), _) -> constrain_identifier environment id
      | (Texp_apply (e1, exps), _) -> constrain_application environment e1 exps
      | (Texp_let (recflag, bindings, body_exp), t) -> constrain_let environment recflag bindings body_exp
      | (Texp_array es, _) -> constrain_array environment es
      | (Texp_sequence (e1, e2), _) -> constrain_sequence environment e1 e2
      | (Texp_tuple es, _) -> constrain_tuple environment es
      | (Texp_assertfalse, _) -> constrain_assertfalse environment e
      | (Texp_assert e, _) -> constrain_assert environment e
      | (_, t) ->
        (* As it turns out, giving up and returning true here is actually _very_ unsound!  We won't check subexpressions! *)
        fprintf err_formatter "@[Warning: Don't know how to constrain expression,
        structure:@ %a@ location:@ %a@]@.@." Printtyp.raw_type_expr t Location.print e.exp_loc; flush stderr;
        assert false
  in log_frame e.exp_loc f; (f, (List.map (label_constraint e) cstrs) @ rec_cstrs)

and constrain_constant path = function
  | Const_int n ->
      (B.mk_int [B.equality_qualifier (P.PInt n)], [], [])
  | Const_float _ -> (B.uFloat, [], [])
  | _ -> assert false

and constrain_constructed (env, guard, f) tyenv cstrdesc args = match f with
  | F.Fconstr (path, tyargframes, cstrs, _) ->
      let tag = cstrdesc.cstr_tag in
      let cstrref = match tag with
        | Cstr_constant n | Cstr_block n -> B.tag_refinement n
        | Cstr_exception _ -> assert false
      in
      let f = F.Fconstr (path, tyargframes, cstrs, cstrref) in
      let argtuple = F.fresh_constructor tyenv cstrdesc f in
      let (argframes, argcstrs) = constrain_subexprs env guard args in
        (f, [SubFrame(env, guard, F.Ftuple argframes, argtuple);
             SubFrame(env, guard, F.Ftuple tyargframes, F.Ftuple tyargframes);
             WFFrame(env, f)], argcstrs)
  | _ -> assert false

and constrain_record (env, guard, f) labeled_exprs =
  let compare_labels ({lbl_pos = n}, _) ({lbl_pos = m}, _) = compare n m in
  let (_, sorted_exprs) = List.split (List.sort compare_labels labeled_exprs) in
  let (subframes, subexp_cs) = constrain_subexprs env guard sorted_exprs in
  let subframe_field cs_rest fsub (fsup, _, _) = SubFrame (env, guard, fsub, fsup) :: cs_rest in
  match f with
    | F.Frecord (p, recframes, _) ->
      let field_qualifier (_, name, _) fexpr = B.field_eq_qualifier name (expression_to_pexpr fexpr) in
        (F.Frecord (p, recframes, ([], F.Qconst (List.map2 field_qualifier recframes sorted_exprs))),
         WFFrame (env, f) :: List.fold_left2 subframe_field [] subframes recframes,
         subexp_cs)
    | _ -> assert false

and constrain_field (env, guard, _) expr label_desc =
  let (recframe, cstrs) = constrain expr env guard in
  let (fieldframe, fieldname) = match recframe with
    | F.Frecord (_, fs, _) -> (match List.nth fs label_desc.lbl_pos with (fr, name, _) -> (fr, name))
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

and constrain_function (env, guard, f) t pat e' =
  match f with
    | (F.Farrow (_, f, unlabelled_f')) ->
      let _ =
        match (t.desc, pat.pat_desc) with
            (* pmr: needs to be generalized to match other patterns *)
          | (Tarrow(_, t_in, t_out, _), Tpat_var x) ->
              ()
          | _ -> ()
      in
      let env' = Pattern.env_bind env pat.pat_desc f in
      let (f'', cstrs) = constrain e' env' guard in
        (* Since the underlying type system doesn't have dependent
           types, fresh can't give us the proper labels for the RHS.
           Instead, we have to label it after the fact. *)
      let f' = F.label_like unlabelled_f' f'' in
      let f = F.Farrow (Some pat.pat_desc, f, f') in
        (f, [WFFrame (env, f); SubFrame (env', guard, f'', f')], cstrs)
    | _ -> assert false

and constrain_base_identifier env id e =
  (F.apply_refinement (B.equality_refinement (expression_to_pexpr e)) (Le.find id env), [], [])

and constrain_identifier (env, guard, ftemplate) id =
  let f' = try Le.find id env with Not_found -> fprintf std_formatter "@[Not_found:@ %s@]" (Path.unique_name id); raise Not_found in
  let f = F.instantiate f' ftemplate in (f, [SubFrame(env, guard, f, f); WFFrame(env, f)], [])

and apply_once env guard (f, cstrs, subexp_cstrs) e = match (f, e) with
  | (F.Farrow (l, f, f'), (Some e2, _)) ->
    let (f2, e2_cstrs) = constrain e2 env guard in
    let f'' = match l with
      | Some pat ->
        (* We _must_ apply a substitution over the whole pattern or
           else we risk capturing stuff in the environment (e.g,
           apply (a, b) -> v > a in the context where a is defined).
           This risks severe unsoundness. *)
        List.fold_right F.apply_substitution (Pattern.bind_pexpr pat (expression_to_pexpr e2)) f'
          (* pmr: The soundness of this next line is suspect,
             must investigate (i.e., what if there's a var that might
             somehow be substituted that isn't because of this?  How
             does it interact w/ the None label rules for subtyping? *)
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
  let (f, fs) =
    (match f with
      | F.Fconstr(p, l, varis, _) -> (F.Fconstr(p, l, varis, B.size_lit_refinement(List.length elements)), l)
      | _ -> assert false) in
  let list_rec (fs, c) e = (fun (f, cs) -> (f::fs, cs @ c)) (constrain e env guard) in
  let (fs', sub_cs) = List.fold_left list_rec ([], []) elements in
  let mksub b a = SubFrame(env, guard, a, b) in
    (f, WFFrame(env, f) :: List.map (mksub (List.hd fs)) fs', sub_cs)

and constrain_sequence (env, guard, _) e1 e2 =
  let (_, cs1) = constrain e1 env guard in
  let (f, cs2) = constrain e2 env guard in (f, [], cs1 @ cs2)

and constrain_tuple (env, guard, f) es =
  let (fs, subexp_cs) = constrain_subexprs env guard es in
  match f with
    | F.Ftuple fresh_fs ->
        let new_cs = List.fold_left2
          (fun cs rec_frame fresh_frame ->
            WFFrame (env, fresh_frame) :: SubFrame (env, guard, rec_frame, fresh_frame) :: cs)
          [] fs fresh_fs
        in (f, WFFrame (env, f) :: new_cs, subexp_cs)
    | _ -> assert false

and constrain_assertfalse (env, _, _) e = (F.fresh_unconstrained e.exp_env e.exp_type, [], [])

and constrain_assert (env, guard, _) e =
  let (f, cstrs) = constrain e env guard in
  let guardvar = Path.mk_ident "assert_guard" in
  let env = Le.add guardvar f env in
  let assert_qualifier =
    (Path.mk_ident "assertion",
     Path.mk_ident "null",
     P.equals (P.Var guardvar, P.int_true))
  in (B.mk_unit (), [SubFrame (env, guard, B.mk_int [], B.mk_int [assert_qualifier])], cstrs)

and constrain_and_bind guard (env, cstrs) (pat, e) =
  let (f, cstrs') = constrain e env guard in
  let env = Pattern.env_bind env pat.pat_desc f in
  (env, cstrs @ cstrs')

and constrain_bindings env guard recflag bindings =
  match recflag with
  | Default
  | Nonrecursive -> List.fold_left (constrain_and_bind guard) (env, []) bindings
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
    let no_label_frame e = F.fresh e.exp_env e.exp_type in
    let unlabeled_frames = List.map no_label_frame exprs in
    let binding_var = function
    | {pat_desc = Tpat_var f} -> Path.Pident f
    | _ -> assert false
    in
    let vars = List.map binding_var vars in
    let unlabeled_env = Le.addn (List.combine vars unlabeled_frames) env in
    let labeling_constraints = List.map (fun e -> constrain e unlabeled_env guard) exprs in
    let (label_frames, _) = List.split labeling_constraints in
    (* pmr: I'm not assuming that constrain always gives us a fresh frame here, otherwise we could
     use label_frames directly *)
    let binding_frames = List.map2 F.label_like unlabeled_frames label_frames in
            
    (* Redo constraints now that we know what the right labels are --- note that unlabeled_frames are all
     still essentially fresh, since we're discarding any constraints on them *)
    let bound_env = Le.addn (List.combine vars binding_frames) env in
    let found_frame_list e b = 
        subexpr_folder bound_env guard e b in
                  
    let (found_frames, subexp_cstrs) = List.fold_right found_frame_list exprs ([], []) in
    (* Ensure that the types we discovered for each binding are no more general than the types implied by
     their uses *)
    (* pmr: This is going to take some work to resolve; for now, default on the locations because we don't
       have anything that'll butt up against it. *)
    let make_cstr fc = {lc_cstr = fc; lc_orig = Loc Location.none; lc_id = fresh_fc_id ()} in
    let build_found_frame_cstr_list cs found_frame binding_frame =
      make_cstr (WFFrame (bound_env, binding_frame)) ::
      make_cstr (SubFrame (bound_env, guard, found_frame, binding_frame)) :: cs
    in (bound_env, (List.fold_left2 build_found_frame_cstr_list [] found_frames binding_frames) @ subexp_cstrs)

and subexpr_folder subenv guard e (fframes, cs) =
  let (frame, new_cs) = constrain e subenv guard in (frame :: fframes, new_cs @ cs)

and constrain_subexprs subenv guard exprs = List.fold_right (subexpr_folder subenv guard) exprs ([], [])

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
        (*Printf.printf "Ignoring type decl";*) constrain_rec quals fenv cstrs srem
    | _ -> assert false
  in constrain_rec initquals initfenv [] str

module QualifierSet = Set.Make(Qualifier)

(* Make copies of all the qualifiers where the free identifiers are replaced
   by the appropriate bound identifiers from the environment. *)
let instantiate_in_environments cs qs =
  let envs = List.map (fun c -> match c.lc_cstr with SubFrame (e,_,_,_) | WFFrame (e,_) -> e) cs in
  let instantiate_qual qualset q =
    let instantiate_in_env qset env =
      try
        QualifierSet.add (Qualifier.instantiate env q) qset
      with Qualifier.Refinement_not_closed -> qset
    in List.fold_left instantiate_in_env qualset envs
  in QualifierSet.elements (List.fold_left instantiate_qual QualifierSet.empty qs)

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

let pre_solve () = 
  C.cprintf C.ol_solve_master "@[##solve##@\n@]"; Bstats.reset ()

let post_solve () = 
  if C.ck_olev C.ol_timing then
    (Printf.printf "##time##\n"; Bstats.print stdout "\nTime to solve constraints:\n";
    Printf.printf "##endtime##\n"; (*TheoremProver.dump_simple_stats ()*))

let qualify_implementation sourcefile fenv qs str =
  let (qs, _, cs) = constrain_structure fenv qs str in
  let inst_qs = instantiate_in_environments cs qs in
  let _ = pre_solve () in
  let (s,cs) = Bstats.time "solving" (solve inst_qs) cs in
  let _ = post_solve () in
  let _ = dump_frames sourcefile (framemap_apply_solution s !flog) in
  match cs with [] -> () | _ ->
    (Printf.printf "Errors encountered during type checking:\n\n";
    flush stdout; raise (Errors(List.map (make_frame_error s) cs)))
