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

open Parsetree (* must be opened before typedtree *)
open Types
open Typedtree
open Btype
open Format
open Asttypes

module C = Common
module PM = C.PathMap
module T = Typetexp
module Pat = Pattern

(**************************************************************)
(************** Type definitions: Refinements *****************)
(**************************************************************)

type substitution = Path.t * Predicate.pexpr

type open_assignment = Top | Bottom

type qvar = Path.t * open_assignment
type refexpr = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

(**************************************************************)
(***************** Type definitions: Frames *******************)
(**************************************************************)

type t =
  | Fvar of Path.t * refinement
  | Fconstr of Path.t * recvar * constr list * refinement
  | Fabstract of Path.t * param list * refinement
  | Farrow of pattern_desc option * t * t
  | Funknown

and param = Ident.t * t * variance

and constr = constructor_tag * param list

and variance = Covariant | Contravariant | Invariant

and recvar = Path.t option

let path_tuple = Path.mk_ident "tuple"

(**************************************************************)
(**************** Constructed type accessors ******************)
(**************************************************************)

let params_frames ps =
  List.map (fun (_, f, _) -> f) ps

let constrs_params cs =
  C.flap snd cs

let constrs_param_frames cs =
  params_frames (constrs_params cs)

(**************************************************************)
(************************* Iterators **************************) 
(**************************************************************)

let map_params f ps =
  List.map (fun (p, fr, v) -> (p, f fr, v)) ps

let rec map f = function
  | (Funknown | Fvar _) as fr -> f fr
  | Fconstr (p, rv, cs, r) ->
      f (Fconstr (p, rv, List.map (fun (cd, ps) -> (cd, map_params f ps)) cs, r))
  | Fabstract (p, ps, r) -> f (Fabstract (p, map_params f ps, r))
  | Farrow (x, f1, f2) -> f (Farrow (x, map f f1, map f f2))

let rec map_refinements_map f = function
  | Fvar (p, r) -> Fvar (p, f r)
  | Fconstr (p, rv, cs, r) -> Fconstr (p, rv, cs, f r)
  | Fabstract (p, ps, r) -> Fabstract (p, ps, f r)
  | f -> f

let map_refinements f fr =
  map (map_refinements_map f) fr

let map_refexprs f fr =
  map_refinements (fun r -> List.map f r) fr

let rec refinement_fold f l = function
  | Fvar (_, r) -> f r l
  | Fconstr (_, _, cs, r) ->
      f r (List.fold_left (refinement_fold f) l (constrs_param_frames cs))
  | Fabstract (_, ps, r) ->
      f r (List.fold_left (refinement_fold f) l (params_frames ps))
  | _ -> l

(**************************************************************)
(****************** Refinement manipulation *******************) 
(**************************************************************)

let mk_refinement subs qconsts qvars =
  [(subs, (qconsts, qvars))]

let empty_refinement =
  mk_refinement [] [] []

let false_refinement =
  mk_refinement [] [(Path.mk_ident "false", Path.mk_ident "V", Predicate.Not (Predicate.True))] []

let apply_refinement r = function
  | Fvar (p, _) -> Fvar (p, r)
  | Fconstr (p, rv, cs, _) -> Fconstr (p, rv, cs, r)
  | Fabstract (p, ps, _) -> Fabstract (p, ps, r)
  | f -> f

let get_refinement = function
  | Fvar (_, r) | Fconstr (_, _, _, r) | Fabstract (_, _, r) -> Some r
  | _ -> None

let append_refinement res' f =
  match get_refinement f with
    | Some res -> apply_refinement (res @ res') f
    | None -> f

let apply_substitution_map sub (subs, qe) =
  (sub :: subs, qe)

let apply_substitution sub f =
  map_refexprs (apply_substitution_map sub) f

let refinement_qvars r =
  C.flap (fun (_, (_, qvars)) -> qvars) r

let qvars f =
  refinement_fold (fun r vs -> refinement_qvars r @ vs) [] f

(**************************************************************)
(*********** Conversions to/from simple refinements ***********)
(**************************************************************)

let split_refexpr (qc, qv) (subs, (qconsts, qvars)) =
  (qc @ List.map (fun q -> (subs, Qconst q)) qconsts,
   qv @ List.map (fun k -> (subs, Qvar k)) qvars)

let ref_to_simples r =
  List.fold_left split_refexpr ([], []) r

let ref_of_simple = function
  | (subs, Qconst q) -> mk_refinement subs [q] []
  | (subs, Qvar v) -> mk_refinement subs [] [v]

(**************************************************************)
(************************* Shapes *****************************) 
(**************************************************************)

let same_shape t1 t2 =
  let vars = ref [] in
  let ismapped p q = try snd (List.find (fun (p', q) -> Path.same p p') !vars) = q with
      Not_found -> vars := (p, q) :: !vars; true in
  let rec sshape = function
      (Fconstr(p, rv, cs, _), Fconstr(p', rv', cs', _)) ->
        Path.same p p' && params_sshape (constrs_params cs) (constrs_params cs') &&
          (rv = rv' || match (rv, rv') with (Some rp, Some rp') -> ismapped rp rp' | _ -> false)
    | (Fabstract(p, ps, _), Fabstract(p', ps', _)) ->
        Path.same p p' && params_sshape ps ps'
    | (Fvar (p, _), Fvar (p', _)) ->
        ismapped p p'
    | (Farrow(_, i, o), Farrow(_, i', o')) ->
        sshape (i, i') && sshape (o, o')
    | (Funknown, Funknown) -> true
    | t -> false
  and params_sshape ps qs =
    List.for_all sshape (List.combine (params_frames ps) (params_frames qs))
  in sshape (t1, t2)

(******************************************************************************)
(********************************** Unfolding *********************************)
(******************************************************************************)

let unfold f = match f with
  | Fconstr (_, Some p, _, _) -> map (function Fvar (p', _) when Path.same p p' -> f | f -> f) f
  | _                          -> f

(**************************************************************)
(******************** Frame pretty printers *******************)
(**************************************************************)

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Path.name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pred_of_qual q =
  Qualifier.apply (Predicate.Var (Path.mk_ident "V")) q

let space ppf =
  fprintf ppf " "

let pprint_refexpr ppf (subs, (qconsts, qvars)) =
  fprintf ppf "%a@;<1 0>%a"
    (Oprint.print_list (fun ppf q -> Predicate.pprint ppf
                          (Predicate.apply_substs subs (pred_of_qual q))) space) qconsts
    (Oprint.print_list (fun ppf (v, _) -> fprintf ppf "%s" (C.path_name () v)) space) qvars

let pprint_refinement ppf res =
  Oprint.print_list pprint_refexpr space ppf res

let rec pprint_pattern ppf = function
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var x -> fprintf ppf "%s" (Ident.name x)
  | Tpat_tuple pats ->
      fprintf ppf "(%a)" pprint_pattern_list pats
  | Tpat_construct (cstrdesc, pats) ->
      begin match (repr cstrdesc.cstr_res).desc with
        | Tconstr (p, _, _) -> fprintf ppf "%s(%a)" (Path.name p) pprint_pattern_list pats
        | _ -> assert false
      end
  | _ -> assert false

and pprint_pattern_list ppf pats =
  Oprint.print_list pprint_pattern (fun ppf -> fprintf ppf ", ") ppf (List.map (fun p -> p.pat_desc) pats)

let wrap_refined ppf pp r =
  if List.for_all (function (_, ([], [])) -> true | _ -> false) r then
    pp ppf
  else
    (fprintf ppf "@[{"; pp ppf; fprintf ppf " |@;<1 2>%a}@]" pprint_refinement r)

let rec pprint ppf = function
  | Fvar (a, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "'%s" (C.path_name () a)) r
  | Fconstr (path, _, [], r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () path)) r
  | Fconstr (path, rv, cs, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%s%s @[(%a)@]@]"
                          (match rv with Some rp -> "μ" ^ C.path_name () rp ^ ". " | None -> "")
                          (C.path_name () path)
                          (Oprint.print_list pprint_constructor
                             (fun ppf -> fprintf ppf "@;<1 0>|| ")) cs) r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some pat, f, f') ->
      fprintf ppf "@[%a:@ %a@ ->@;<1 2>%a@]" pprint_pattern pat pprint1 f pprint f'
  | Fabstract (path, params, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a@ %s@]"
                          (pprint_params ",") params
                          (C.path_name () path)) r
  | Funknown ->
      fprintf ppf "[unknown]"
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f
 and pprint_constructor ppf (_, ps) =
    pprint_params "*" ppf ps
 and pprint_param ppf (name, f, _) =
  fprintf ppf "%s:@;<1 2>%a" (C.ident_name name) pprint f
 and pprint_params sep ppf ps =
  Oprint.print_list pprint_param (fun ppf -> fprintf ppf "@;<1 2>%s@;<1 2>" sep) ppf ps

let rec pprint_fenv ppf fenv =
  Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (C.path_name () k) pprint v) fenv

(**************************************************************)
(********* Polymorphic and qualifier instantiation ************) 
(**************************************************************)

(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate fr ftemplate =
  let vars = ref [] in
  let rec inst f ft =
    match (f, ft) with
      | (Fvar (p, r), _) ->
          let instf =
            try List.assoc p !vars with Not_found -> vars := (p, ft) :: !vars; ft
          in append_refinement r instf
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
      | (Fconstr (p, _, cs, r), Fconstr(p', rv', cs', _)) ->
          Fconstr(p, rv', List.map2 (fun (cd, ps) (_, ps') -> (cd, inst_params ps ps')) cs cs', r)
      | (Fabstract(p, ps, r), Fabstract(_, ps', _)) ->
          Fabstract(p, inst_params ps ps', r)
      | (Funknown, Funknown) -> Funknown
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false
  and inst_params ps ps' =
    List.map2 (fun (p, f, v) (_, f', _) -> (p, inst f f', v)) ps ps'
  in inst fr ftemplate

let instantiate_qualifiers_map vars (subs, (qconsts, qvars)) =
  (subs, (List.map (fun q -> match Qualifier.instantiate vars q with Some q -> q | None -> q) qconsts,
          qvars))

let instantiate_qualifiers vars fr =
  map_refexprs (instantiate_qualifiers_map vars) fr

(**************************************************************)
(********************* Argument labeling **********************) 
(**************************************************************)

(* Label all the function formals in [f] with their corresponding labels in
   [f'] and changing constant qualifiers appropriately.
   [f] and [f'] are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let label_like f f' =
  let rec label vars f f' = match (f, f') with
    | (Fvar _, Fvar _) | (Funknown, Funknown)
    | (Fconstr _, Fconstr _) | (Fabstract _, Fabstract _) ->
        instantiate_qualifiers vars f
    | (Farrow (None, f1, f1'), Farrow (l, f2, f2')) ->
        Farrow (l, label vars f1 f2, label vars f1' f2')
    | (Farrow (Some p1, f1, f1'), Farrow (Some p2, f2, f2')) ->
        let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
          Farrow (Some p2, label vars f1 f2, label vars' f1' f2')
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  in label [] f f'

let record_of_params path ps r =
  Fconstr(path, None, [(Cstr_constant 0, ps)], r)

let tuple_of_frames fs r =
  record_of_params path_tuple (Misc.mapi (fun f i -> (C.tuple_elem_id i, f, Covariant)) fs) r

(**************************************************************)
(******************* Fresh frames *****************************)
(**************************************************************)

let translate_variance = function
  | (true, true, true) -> Invariant
  | (true, false, false) -> Covariant
  | (false, true, false) -> Contravariant
  | _ -> assert false

let mutable_variance = function
  | Mutable -> Invariant
  | _ -> Covariant

let fresh_refinementvar open_assn () =
  mk_refinement [] [] [(Path.mk_ident "k", open_assn)]

(* pmr: this looks suspect - ming? isn't this just empty_refinement? *)
let fresh_true () =
  mk_refinement [] [(C.dummy (), Path.mk_ident "true", Predicate.True)] []

let fresh_fvar () = Fvar(Path.mk_ident "a", empty_refinement)

let rec canonicalize t =
  let t = repr t in
    t.level <- 0;
    begin match t.desc with
      | Tconstr _ -> t.id <- 0
      | _         -> ()
    end; Btype.iter_type_expr (fun t -> ignore (canonicalize t)) t; t

let close_arg res arg =
  if res.desc = arg.desc then link_type arg res

let close_constructor res args =
  List.iter (fun a -> Btype.iter_type_expr (close_arg res) a; close_arg res a) args

let fresh_constr fresh env p t tyl =
  let params  = List.map (fresh []) tyl in
  let ty_decl = Env.find_type p env in
  (* Used in the sum type case - the type constructors contain uninstantiated
     types, which we need to map to the instantiated type parameters. This is rather
     easier than unification, which requires a type environment and more logic. *)
  let pm      = List.combine ty_decl.type_params params in
  let varis   = List.map translate_variance ty_decl.type_variance in
    match ty_decl.type_kind with
      | Type_abstract ->
          Fabstract (p, C.combine3 (Misc.mapi (fun _ i -> C.tuple_elem_id i) tyl) params varis, empty_refinement)
      | Type_record (fields, _, _) -> (* 1 *)
          let fresh_field (name, muta, t) =
            (Ident.create name, fresh pm t, mutable_variance muta)
          in record_of_params p (List.map fresh_field fields) empty_refinement
      | Type_variant _ ->
          let param_vars = List.combine tyl varis in
          let (_, cds)   = List.split (Env.constructors_of_type p (Env.find_type p env)) in
          let fresh_cstr cstr =
            let (args, res) = Ctype.instance_constructor cstr in
            let _ = close_constructor res args; Ctype.unify Env.initial res t in
            let (res, args) = (canonicalize res, List.map canonicalize args) in
            let ids  = Misc.mapi (fun _ i -> C.tuple_elem_id i) args in
            let fs   = List.map (fresh []) args in
            let vs   = List.map (fun t -> try List.assoc (canonicalize t) param_vars with Not_found -> Covariant) args in
              (cstr.cstr_tag, C.combine3 ids fs vs)
          in Fconstr (p, None, List.map fresh_cstr cds, empty_refinement)

let close_recf rv = function
  | Fconstr (p, _, cs, r) -> let f = Fconstr (p, Some rv, cs, r) in if unfold f = f then Fconstr (p, None, cs, r) else f
  | f                     -> f

let fresh_rec fresh env rv t = match t.desc with
  | Tvar                 -> fresh_fvar ()
  | Tarrow(_, t1, t2, _) -> Farrow (None, fresh [] t1, fresh [] t2)
  | Ttuple ts            -> tuple_of_frames (List.map (fresh []) ts) empty_refinement
  | Tconstr(p, tyl, _)   -> close_recf rv (fresh_constr fresh env p t (List.map canonicalize tyl))
  | _                    -> fprintf err_formatter "@[Warning: Freshing unsupported type]@."; Funknown

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun env freshf t =
  let tbl = Hashtbl.create 17 in
  let rec fm pmap t =
    let t = canonicalize t in
      List.iter (fun (t, f) -> Hashtbl.replace tbl (canonicalize t) f) pmap;
      let f =
        if Hashtbl.mem tbl t then
          Hashtbl.find tbl t
        else begin
          let rv = Path.mk_ident "t" in
            Hashtbl.replace tbl t (Fvar (rv, empty_refinement));
            let res = fresh_rec fm env rv t in Hashtbl.replace tbl t res; res
        end
      in List.iter (fun (t, _) -> Hashtbl.remove tbl (canonicalize t)) pmap; f
  in map_refinements (fun _ -> freshf ()) (fm [] t)

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations.

   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh env ty =
  fresh_with_var_fun env (fresh_refinementvar Top) ty

(* Create a fresh frame with the same shape as [exp]'s type and [f],
   and the same labels as [f]. *)
let fresh_with_labels env ty f =
  label_like (fresh env ty) f

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars env ty =
  fresh_with_var_fun env (fun _ -> empty_refinement) ty

let fresh_unconstrained env ty =
  fresh_with_var_fun env (fresh_refinementvar Bottom) ty

(**************************************************************)
(********************* mlq translation ************************) 
(**************************************************************)

let transl_pref plist env p = 
  let fp s = 
    let b = try List.find (fun (nm, _) -> nm = s) plist with
      Not_found -> failwith (Printf.sprintf "%s not found in mlq\n" s) in
    (fun (_, p) -> p) b in
  let (v, p) =
    match p with
    | RLiteral (v, p) -> (v, p)
    | RVar s -> fp s in
  let valu = Path.mk_ident v  in
  [([], ([(C.dummy (), valu, Qualdecl.transl_patpred_single false valu env p)], []))]

let rec translate_pframe env plist pf =
  let vars = ref [] in
  let getvar a = try List.find (fun b -> Path.name b = a) !vars
                   with Not_found -> let a = Path.mk_ident a in
                   let _ = vars := a::!vars in
                     a in
  let transl_pref = transl_pref plist env in
  let rec transl_pframe_rec pf =
    match pf with
    | PFvar (a, r) -> Fvar (getvar a, empty_refinement)
    | PFconstr (l, fs, r) -> transl_constr l fs r
    | PFarrow (v, a, b) ->
        let pat = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem (Path.Pident id) !vars then failwith "Redefined variable";
                vars := Path.Pident id :: !vars; Some (Tpat_var id)
          | None -> None
        in Farrow (pat, transl_pframe_rec a, transl_pframe_rec b)
    | PFtuple (fs, r) -> tuple_of_frames (List.map transl_pframe_rec fs) (transl_pref r)
    | PFrecord (fs, r) -> transl_record fs r 
  and transl_constr l fs r =
(*    let (path, decl) = try Env.lookup_type l env with
      Not_found -> raise (T.Error(Location.none, T.Unbound_type_constructor l)) in
    let _ = if List.length fs != decl.type_arity then
      raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
    let fs = List.map transl_pframe_rec fs in
    let fresh freshf ty = fresh_with_var_fun (ref []) env ty freshf in
    let id = (fun f -> f) in
    let refinement () = transl_pref r in *)
      assert false
      (*fresh_constr fresh_true refinement path decl id id fs fresh *)
  and transl_record fs r =
    let ps = List.map (fun (f, s, m) -> (Ident.create s, transl_pframe_rec f, mutable_variance m)) fs in
    let path = Path.mk_ident "_anon_record" in
      record_of_params path ps (transl_pref r)
  in transl_pframe_rec pf

(**************************************************************)
(********************* Pattern binding ************************) 
(**************************************************************)

let bind env pat frame =
  let _bind = function
    | (Tpat_any, _) -> ([], [])
    | (Tpat_var x, f) -> ([], [(Path.Pident x, f)])
    | (Tpat_tuple pats, Fconstr (_, _, [(_, ps)], _)) ->
        (List.combine (Pattern.pattern_descs pats) (params_frames ps), [])
    | (Tpat_construct (cstrdesc, pats), Fconstr (p, _, cfvs, _)) ->
        (List.combine (Pattern.pattern_descs pats) (params_frames (List.assoc cstrdesc.cstr_tag cfvs)), [])
    | _ -> assert false
  in C.expand _bind [(pat, frame)] []

let env_bind tenv env pat frame =
  Lightenv.addn (bind tenv pat frame) env

(**************************************************************)
(******************** Logical embedding ***********************) 
(**************************************************************)

let refexpr_apply_solution solution (subs, (qconsts, qvars)) =
  (subs, (qconsts @ C.flap (fun (k, _) -> solution k) qvars, []))

let apply_solution solution f =
  map_refexprs (refexpr_apply_solution solution) f

let refexpr_conjuncts solution qual_expr ((subs, qexprs) as r) =
  let (_, (quals, _)) = refexpr_apply_solution solution r in
  let unsubst = List.map (Qualifier.apply qual_expr) quals in
    List.map (Predicate.apply_substs subs) unsubst

let refinement_conjuncts solution qual_expr res =
  C.flap (refexpr_conjuncts solution qual_expr) res

let refinement_predicate solution qual_var refn =
  Predicate.big_and (refinement_conjuncts solution qual_var refn)

let rec conjunct_fold cs solution qual_expr = function
  | Fvar(_, r) | Fconstr(_, _, _, r) | Fabstract(_, _, r) ->
      refinement_conjuncts solution qual_expr r @ cs
  | _ -> cs

let rec conjuncts solution qual_expr fr =
  conjunct_fold [] solution qual_expr fr

let predicate solution qual_expr f =
  Predicate.big_and (conjuncts solution qual_expr f)
