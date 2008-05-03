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

type substitution = Path.t * Predicate.pexpr

type open_assignment = Top | Bottom

type qvar = Path.t * open_assignment
type refinement = substitution list * (Qualifier.t list * qvar list)

type t =
  | Fvar of Path.t * refinement
  | Fconstr of Path.t * t list * variance list * refinement
  | Farrow of pattern_desc option * t * t
  | Ftuple of t list * refinement
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

and variance = Covariant | Contravariant | Invariant

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Path.name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pred_of_qual q =
  Qualifier.apply (Predicate.Var (Path.mk_ident "V")) q

let space ppf =
  fprintf ppf " "

let pprint_refinement ppf (subs, (qconsts, qvars)) =
  fprintf ppf "%a@;<1 0>%a"
    (Oprint.print_list (fun ppf q -> Predicate.pprint ppf
                          (Predicate.apply_substs subs (pred_of_qual q))) space) qconsts
    (Oprint.print_list (fun ppf (v, _) -> fprintf ppf "%s" (C.path_name () v)) space) qvars

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

let wrap_refined ppf pp = function
  | (_, ([], [])) -> pp ppf
  | r -> fprintf ppf "@[{"; pp ppf; fprintf ppf " |@;<1 2>%a}@]" pprint_refinement r

let rec pprint ppf = function
  | Fvar (a, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "'%s" (C.path_name () a)) r
  | Fconstr (path, [], _, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () path)) r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some pat, f, f') ->
      fprintf ppf "@[%a:@ %a@ ->@;<1 2>%a@]" pprint_pattern pat pprint1 f pprint f'
  | Fconstr (path, l, _, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%a@ %s" (pprint_list ",") l (C.path_name () path)) r
  | Ftuple (ts, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "(%a)" (pprint_list "*") ts) r
  | Frecord (id, _, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () id)) r
  | Funknown ->
      fprintf ppf "[unknown]"
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f
 and pprint_list sep ppf = Oprint.print_list pprint (fun ppf -> fprintf ppf "@;<1 2>%s@;<1 2>" sep) ppf

let rec pprint_fenv ppf fenv =
  Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (C.path_name () k) pprint v) fenv

let translate_variance = function
  | (true, true, true) -> Invariant
  | (true, false, false) -> Covariant
  | (false, true, false) -> Contravariant
  | _ -> assert false

let rec same_shape map_vars t1 t2 =
  let vars = ref [] in
  let ismapped p q = try snd (List.find (fun (p', q) -> Path.same p p') !vars) = q with
      Not_found -> vars := (p, q) :: !vars; true in
  match (t1, t2) with
  (Fconstr(p, l, _, _), Fconstr(p', l', _, _)) ->
   (Path.same p p') && (List.for_all (fun f -> f) (List.map2 (same_shape map_vars) l l')) 
  | (Fvar (p, _), Fvar (p', _)) ->
   if map_vars then ismapped p p' else Path.same p p'
  | (Farrow(_, i, o), Farrow(_, i', o')) ->
   ((same_shape map_vars) i i') && ((same_shape map_vars) o o')
  | (Ftuple (t1s, _), Ftuple (t2s, _)) ->
   List.for_all2 (same_shape map_vars) t1s t2s
  | (Frecord (p1, f1s, _), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
      let shape_rec (f1, _, _) (f2, _, _) = (same_shape map_vars) f1 f2 in
        List.for_all2 shape_rec f1s f2s
  | (Funknown, Funknown) -> true
  | t -> false 

let empty_refinement = ([], ([], []))

let false_refinement = ([], ([(Path.mk_ident "false", Path.mk_ident "V", Predicate.Not (Predicate.True))], []))

let rec map f = function
    | (Funknown | Fvar _) as fr -> f fr
    | Fconstr (p, fs, cstrdesc, r) -> f (Fconstr (p, List.map (map f) fs, cstrdesc, r))
    | Ftuple (fs, r) -> f (Ftuple (List.map (map f) fs, r))
    | Farrow (x, f1, f2) -> f (Farrow (x, map f f1, map f f2))
    | Frecord (p, fs, r) -> f (Frecord (p, List.map (fun (fr, n, m) -> (map f fr, n, m)) fs, r))

let rec map_refinements_map f = function
  | Fconstr (p, fs, cstrdesc, r) -> Fconstr (p, fs, cstrdesc, f r)
  | Ftuple (fs, r) -> Ftuple (fs, f r)
  | Frecord (p, fs, r) -> Frecord (p, fs, f r)
  | f -> f

let map_refinements f fr = map (map_refinements_map f) fr

(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate fr ftemplate =
  let vars = ref [] in
  let rec inst f ft =
    match (f, ft) with
      | (Fvar _, _) -> (try List.assq f !vars with Not_found -> vars := (f, ft) :: !vars; ft)
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
      | (Fconstr (p, l, varis, r), Fconstr(p', l', _, _)) ->
          Fconstr(p, List.map2 inst l l', varis, r)
      | (Ftuple (t1s, r), Ftuple (t2s, _)) ->
          Ftuple (List.map2 inst t1s t2s, r)
      | (Frecord (p, f1s, r), Frecord (_, f2s, _)) ->
          let inst_rec (f1, name, m) (f2, _, _) = (inst f1 f2, name, m) in
            Frecord (p, List.map2 inst_rec f1s f2s, r)
      | (Funknown, Funknown) -> Funknown
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false
  in inst fr ftemplate

let instantiate_qualifiers_map vars (subs, (qconsts, qvars)) =
  (subs, (List.map (fun q -> match Qualifier.instantiate vars q with Some q -> q | None -> q) qconsts,
          qvars))

let instantiate_qualifiers vars fr =
  map_refinements (instantiate_qualifiers_map vars) fr

let fresh_refinementvar open_assn () = ([], ([], [(Path.mk_ident "k", open_assn)]))
(* pmr: this looks suspect - ming? *)
let fresh_true () = ([], ([(C.dummy (), Path.mk_ident "true", Predicate.True)], []))

let fresh_fvar () = Fvar (Path.mk_ident "a", ([], ([], [])))

(* 1. Tedium ahead:
   OCaml stores information about record types in two places:
    - The type declaration stores everything that's set in stone about the
     type: what its fields are and which variables are the type parameters.
    - The type list of the Tconstr contains the actual instantiations of
     the tyvars in this instantiation of the record. *)

let fresh_constr freshf constrf p ty_decl f g tyl fresh =
  match ty_decl.type_kind with
  | Type_abstract | Type_variant _ ->
    if Path.same p Predef.path_unit then 
      Fconstr(p, [], [], ([], ([], [])))
    else
      Fconstr(p, f tyl, List.map translate_variance ty_decl.type_variance, constrf ())
  | Type_record (fields, _, _) -> (* 1 *)
    let param_map = List.combine ty_decl.type_params tyl in
    let fresh_field (name, muta, typ) =
      let field_typ = try g (List.assoc typ param_map) with 
        Not_found -> fresh freshf typ in
        (field_typ, name, muta) in
    Frecord (p, List.map fresh_field fields, constrf ())

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun vars env ty fresh_ref_var =
  let rec fresh_rec freshf t =
    let t = repr t in
    match t.desc with
        Tvar ->
          (try List.assq t !vars with Not_found ->
            let fv = fresh_fvar () in 
            vars := (t, fv) :: !vars; fv)
      | Tconstr(p, tyl, _) ->
          let ty_decl = Env.find_type p env in
            fresh_constr freshf freshf p ty_decl (List.map (fresh_rec freshf)) (fresh_rec freshf) tyl fresh_rec
      | Tarrow(_, t1, t2, _) -> Farrow (None, fresh_rec freshf t1, fresh_rec freshf t2)
      | Ttuple ts -> Ftuple (List.map (fresh_rec freshf) ts, freshf ())
      | _ -> fprintf err_formatter "@[Warning: Freshing unsupported type]@."; Funknown
  in fresh_rec fresh_ref_var (repr ty)

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations.

   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh env ty = fresh_with_var_fun (ref []) env ty (fresh_refinementvar Top)
let transl_ty env ty = fresh_with_var_fun (ref []) env ty fresh_true

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars env ty = fresh_with_var_fun (ref []) env ty (fun _ -> empty_refinement)

let fresh_unconstrained env ty = fresh_with_var_fun (ref []) env ty (fresh_refinementvar Bottom)

let fresh_constructor env cstrdesc = function
  | Fconstr (_, fl, _, _) ->
      let tyargs = match cstrdesc.cstr_res.desc with Tconstr(_, args, _) -> args | _ -> assert false in
      let argmap = ref (List.combine (List.map repr tyargs) fl) in
        List.map (fun t -> fresh_with_var_fun argmap env t (fresh_refinementvar Top)) cstrdesc.cstr_args
  | _ -> assert false

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
  ([], ([(C.dummy (), valu, Qualdecl.transl_patpred_single false valu env p)], []))

let rec translate_pframe env plist pf =
  let vars = ref [] in
  let getvar a = try List.find (fun b -> Path.name b = a) !vars
                   with Not_found -> let a = Path.mk_ident a in
                   let _ = vars := a::!vars in
                     a in
  let transl_pref = transl_pref plist env in
  let rec transl_pframe_rec pf =
    match pf with
    | PFvar (a, r) -> Fvar (getvar a, ([], ([], [])))
    | PFconstr (l, fs, r) -> transl_constr l fs r
    | PFarrow (v, a, b) ->
        let pat = match v with
            Some id ->
              let id = Ident.create id in
              if List.mem (Path.Pident id) !vars then failwith "Redefined variable";
                vars := Path.Pident id :: !vars; Some (Tpat_var id)
          | None -> None
        in Farrow (pat, transl_pframe_rec a, transl_pframe_rec b)
    | PFtuple (fs, r) -> Ftuple (List.map transl_pframe_rec fs, transl_pref r)
    | PFrecord (fs, r) -> transl_record fs r 
  and transl_constr l fs r =
    let (path, decl) = try Env.lookup_type l env with
      Not_found -> raise (T.Error(Location.none, T.Unbound_type_constructor l)) in
    let _ = if List.length fs != decl.type_arity then
      raise (T.Error(Location.none, T.Type_arity_mismatch(l, decl.type_arity, List.length fs))) in
    let fs = List.map transl_pframe_rec fs in
    let fresh freshf ty = fresh_with_var_fun (ref []) env ty freshf in
    let id = (fun f -> f) in
    let refinement () = transl_pref r in
      fresh_constr fresh_true refinement path decl id id fs fresh
  and transl_record fs r =
    let fs = List.map (fun (f, s, m) -> (transl_pframe_rec f, s, m)) fs in
    let path = Path.mk_ident "_anon_record" in
    Frecord(path, fs, transl_pref r) in
  transl_pframe_rec pf

let bind env pat frame =
  let _bind = function
    | (Tpat_any, _) -> ([], [])
    | (Tpat_var x, f) -> ([], [(Path.Pident x, f)])
    | (Tpat_tuple pats, Ftuple (fs, _)) ->
        (List.combine (Pattern.pattern_descs pats) fs, [])
    | (Tpat_construct (cstrdesc, pats), f) ->
        (List.combine (Pattern.pattern_descs pats) (fresh_constructor env cstrdesc f), [])
    | _ -> assert false
  in C.expand _bind [(pat, frame)] []

let env_bind tenv env pat frame =
  Lightenv.addn (bind tenv pat frame) env

(* Label all the function formals in [f] with their corresponding labels in
   [f'] and changing constant qualifiers appropriately.
   [f] and [f'] are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let label_like f f' =
  let rec label vars f f' = match (f, f') with
    | (Fvar _, Fvar _) | (Funknown, Funknown) | (Fconstr _, Fconstr _) -> instantiate_qualifiers vars f
    | (Farrow (None, f1, f1'), Farrow (l, f2, f2')) ->
        Farrow (l, label vars f1 f2, label vars f1' f2')
    | (Farrow (Some p1, f1, f1'), Farrow (Some p2, f2, f2')) ->
        let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
          Farrow (Some p2, label vars f1 f2, label vars' f1' f2')
    | (Ftuple (t1s, r), Ftuple (t2s, _)) ->
        Ftuple (List.map2 (label vars) t1s t2s, r)
    | (Frecord (p1, f1s, r), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
        let label_rec (f1, n, muta) (f2, _, _) = (label vars f1 f2, n, muta) in
          Frecord (p1, List.map2 label_rec f1s f2s, r)
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  in label [] f f'

(* Create a fresh frame with the same shape as [exp]'s type and [f],
   and the same labels as [f]. *)
let fresh_with_labels env ty f = label_like (fresh env ty) f

let apply_substitution_map sub (subs, qe) =
  (sub :: subs, qe)

let apply_substitution sub f =
  map_refinements (apply_substitution_map sub) f

let refinement_apply_solution solution (subs, (qconsts, qvars)) =
  (subs, (qconsts @ C.flap (fun (k, _) -> solution k) qvars, []))

let apply_solution solution f =
  map_refinements (refinement_apply_solution solution) f

let refinement_conjuncts solution qual_expr ((subs, qexprs) as r) =
  let (_, (quals, _)) = refinement_apply_solution solution r in
  let unsubst = List.map (Qualifier.apply qual_expr) quals in
    List.map (Predicate.apply_substs subs) unsubst

let ref_vars (_, (_, qvars)) =
  qvars

let rec refinement_vars = function
  | Fvar (_, r) -> ref_vars r
  | Fconstr (_, _, _, r) -> ref_vars r
  | Ftuple (fs, r) ->
      (C.flap refinement_vars fs) @ ref_vars r
  | Frecord (_, fs, r) ->
      (C.flap (fun (f, _, _) -> refinement_vars f) fs) @ ref_vars r
  | _ -> []

let apply_refinement r = function
  | Fvar (p, _) -> Fvar (p, r)
  | Fconstr (p, fl, varis, _) -> Fconstr (p, fl, varis, r)
  | Frecord (p, fs, _) -> Frecord (p, fs, r)
  | Ftuple (fs, _) -> Ftuple (fs, r)
  | f -> f

let refinement_predicate solution qual_var refn =
  Predicate.big_and (refinement_conjuncts solution qual_var refn)

let rec conjunct_fold cs solution qual_expr = function
  | Fvar(_, r) -> refinement_conjuncts solution qual_expr r @ cs
  | Fconstr(_, _, _, r) -> refinement_conjuncts solution qual_expr r @ cs
  | Frecord (p, fs, r) ->
      let subframe_fold b (f, name, _) =
        conjunct_fold b solution (Predicate.Field (name, qual_expr)) f
      in refinement_conjuncts solution qual_expr r @ List.fold_left subframe_fold cs fs
  | Ftuple (fs, r) ->
      let subframe_fold i b f =
        conjunct_fold b solution (Predicate.Proj (i, qual_expr)) f
      in refinement_conjuncts solution qual_expr r @ C.fold_lefti subframe_fold cs fs
  | _ -> cs

let rec conjuncts solution qual_expr fr =
  conjunct_fold [] solution qual_expr fr

let predicate solution qual_expr f =
  Predicate.big_and (conjuncts solution qual_expr f)
