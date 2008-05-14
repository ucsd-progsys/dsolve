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
  | Fconstr of Path.t * constr list * refinement
  | Fabstract of Path.t * param list * refinement
  | Farrow of pattern_desc option * t * t
  | Frecord of Path.t * param list * refinement
  | Funknown

and param = Ident.t * t * variance

and constr = constructor_tag * param list

and variance = Covariant | Contravariant | Invariant

let path_tuple = Path.mk_ident "tuple"

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
  | Fconstr (path, [], r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s" (C.path_name () path)) r
  | Fconstr (path, cs, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%s @[(%a)@]@]"
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
  | Frecord (p, ps, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "%s {%a}" (C.path_name () p) (pprint_params "*") ps) r
  | Funknown ->
      fprintf ppf "[unknown]"
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f
 and pprint_constructor ppf (_, ps) = pprint_params "*" ppf ps
 and pprint_param ppf (name, f, _) = fprintf ppf "%s:@;<1 2>%a" (C.ident_name name) pprint f
 and pprint_params sep ppf ps =
  Oprint.print_list pprint_param (fun ppf -> fprintf ppf "@;<1 2>%s@;<1 2>" sep) ppf ps

let rec pprint_fenv ppf fenv =
  Lightenv.maplist (fun k v -> printf "@[%s:@ %a@]@." (C.path_name () k) pprint v) fenv

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
  | Fconstr (p, cs, r) ->
      f (Fconstr (p, List.map (fun (cd, ps) -> (cd, map_params f ps)) cs, r))
  | Fabstract (p, ps, r) -> f (Fabstract (p, map_params f ps, r))
  | Farrow (x, f1, f2) -> f (Farrow (x, map f f1, map f f2))
  | Frecord (p, ps, r) -> f (Frecord (p, map_params f ps, r))

let rec map_refinements_map f = function
  | Fvar (p, r) -> Fvar (p, f r)
  | Fconstr (p, cs, r) -> Fconstr (p, cs, f r)
  | Fabstract (p, ps, r) -> Fabstract (p, ps, f r)
  | Frecord (p, ps, r) -> Frecord (p, ps, f r)
  | f -> f

let map_refinements f fr =
  map (map_refinements_map f) fr

let map_refexprs f fr =
  map_refinements (fun r -> List.map f r) fr

let rec refinement_fold f l = function
  | Fvar (_, r) -> f r l
  | Fconstr (_, cs, r) ->
      f r (List.fold_left (refinement_fold f) l (constrs_param_frames cs))
  | Fabstract (_, ps, r) ->
      f r (List.fold_left (refinement_fold f) l (params_frames ps))
  | Frecord (_, ps, r) ->
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
  | Fconstr (p, cs, _) -> Fconstr (p, cs, r)
  | Fabstract (p, ps, _) -> Fabstract (p, ps, r)
  | Frecord (p, ps, _) -> Frecord (p, ps, r)
  | f -> f

let get_refinement = function
  | Fvar (_, r) | Fconstr (_, _, r) | Fabstract (_, _, r) | Frecord (_, _, r) -> Some r
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
      (Fconstr(p, cs, _), Fconstr(p', cs', _)) ->
        Path.same p p' && params_sshape (constrs_params cs) (constrs_params cs')
    | (Fabstract(p, ps, _), Fabstract(p', ps', _)) ->
        Path.same p p' && params_sshape ps ps'
    | (Fvar (p, _), Fvar (p', _)) ->
        ismapped p p'
    | (Farrow(_, i, o), Farrow(_, i', o')) ->
        sshape (i, i') && sshape (o, o')
    | (Frecord (p1, p1s, _), Frecord (p2, p2s, _)) when Path.same p1 p2 ->
        params_sshape p1s p2s
    | (Funknown, Funknown) -> true
    | t -> false
  and params_sshape ps qs =
    List.for_all sshape (List.combine (params_frames ps) (params_frames qs))
  in sshape (t1, t2)

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
      | (Fconstr (p, cs, r), Fconstr(p', cs', _)) ->
          Fconstr(p, List.map2 (fun (cd, ps) (_, ps') -> (cd, inst_params ps ps')) cs cs', r)
      | (Fabstract(p, ps, r), Fabstract(_, ps', _)) ->
          Fabstract(p, inst_params ps ps', r)
      | (Frecord (p, p1s, r), Frecord (_, p2s, _)) ->
          Frecord (p, inst_params p1s p2s, r)
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
    | (Frecord (p1, p1s, r), Frecord (p2, p2s, _)) when Path.same p1 p2 ->
        let label_rec (p, f1, vari) (_, f2, _) = (p, label vars f1 f2, vari) in
          Frecord (p1, List.map2 label_rec p1s p2s, r)
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  in label [] f f'

let tuple_of_frames fs r =
  Frecord(path_tuple, Misc.mapi (fun f i -> (C.tuple_elem_id i, f, Covariant)) fs, r)

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

(* 1. Tedium ahead:
   OCaml stores information about record types in two places:
    - The type declaration stores everything that's set in stone about the
     type: what its fields are and which variables are the type parameters.
    - The type list of the Tconstr contains the actual instantiations of
     the tyvars in this instantiation of the record. *)

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun vars env ty fresh_ref_var =
  let rec fresh_rec freshf t =
    let t = repr t in
    match t.desc with
        Tvar ->
          let fvar =
            try List.assq t !vars with Not_found ->
              let fv = fresh_fvar () in
                vars := (t, fv) :: !vars; fv
          in apply_refinement (freshf ()) fvar
      | Tconstr(p, tyl, _) ->
          let tyl = List.map repr tyl in
          let params = List.map (fresh_rec freshf) tyl in
          let ty_decl = Env.find_type p env in
          let _ = vars := (List.combine ty_decl.type_params params) @ !vars in
          let varis = List.map translate_variance ty_decl.type_variance in
          begin match ty_decl.type_kind with
            | Type_abstract ->
                let names = Misc.mapi (fun _ i -> C.tuple_elem_id i) tyl in
                  Fabstract(p, C.combine3 names params varis, freshf ())
            | Type_variant _ ->
                (* pmr: for the love of god can we deprecate this case already?! *)
                if Path.same p Predef.path_unit then 
                  Fconstr(p, [(Cstr_constant 0, [])], empty_refinement)
                else
                  let param_vars = List.combine tyl varis in
                  let (_, cds) = List.split (Env.constructors_of_type p (Env.find_type p env)) in
                  let fresh_cstr cstr =
                    (cstr.cstr_tag,
                     Misc.mapi
                       (fun t i -> (C.tuple_elem_id i, fresh_rec freshf t, try List.assoc t param_vars with Not_found -> Covariant))
                       (List.map repr cstr.cstr_args))
                  in Fconstr(p, List.map fresh_cstr cds, freshf())
            | Type_record (fields, _, _) -> (* 1 *)
                (* pmr: this param_map business is probably deprecated *)
                let param_map = List.combine ty_decl.type_params tyl in
                let fresh_field (name, muta, typ) =
                  let field_typ = try fresh_rec freshf (List.assoc typ param_map) with 
                      Not_found -> fresh_rec freshf typ in
                    (Ident.create name, field_typ, mutable_variance muta) in
                  Frecord (p, List.map fresh_field fields, freshf())
          end
      | Tarrow(_, t1, t2, _) -> Farrow (None, fresh_rec freshf t1, fresh_rec freshf t2)
      | Ttuple ts -> tuple_of_frames (List.map (fresh_rec freshf) ts) (freshf ())
      | _ -> fprintf err_formatter "@[Warning: Freshing unsupported type]@."; Funknown
  in fresh_rec fresh_ref_var (repr ty)

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations.

   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh env ty =
  fresh_with_var_fun (ref []) env ty (fresh_refinementvar Top)

(* Create a fresh frame with the same shape as [exp]'s type and [f],
   and the same labels as [f]. *)
let fresh_with_labels env ty f =
  label_like (fresh env ty) f

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars env ty =
  fresh_with_var_fun (ref []) env ty (fun _ -> empty_refinement)

let fresh_unconstrained env ty =
  fresh_with_var_fun (ref []) env ty (fresh_refinementvar Bottom)

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
    Frecord(path, ps, transl_pref r) in
  transl_pframe_rec pf

(**************************************************************)
(********************* Pattern binding ************************) 
(**************************************************************)

let bind env pat frame =
  let _bind = function
    | (Tpat_any, _) -> ([], [])
    | (Tpat_var x, f) -> ([], [(Path.Pident x, f)])
    | (Tpat_tuple pats, Frecord (_, ps, _)) ->
        (List.combine (Pattern.pattern_descs pats) (params_frames ps), [])
    | (Tpat_construct (cstrdesc, pats), Fconstr (p, cfvs, _)) ->
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
  | Fvar(_, r) | Fconstr(_, _, r) | Fabstract(_, _, r) ->
      refinement_conjuncts solution qual_expr r @ cs
  | Frecord (p, fs, r) ->
      let subframe_fold b (id, f, _) =
        conjunct_fold b solution (Predicate.Field (id, qual_expr)) f
      in refinement_conjuncts solution qual_expr r @ List.fold_left subframe_fold cs fs
  | _ -> cs

let rec conjuncts solution qual_expr fr =
  conjunct_fold [] solution qual_expr fr

let predicate solution qual_expr f =
  Predicate.big_and (conjuncts solution qual_expr f)
