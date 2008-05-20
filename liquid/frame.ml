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
module M = Misc

(**************************************************************)
(************** Type definitions: Refinements *****************)
(**************************************************************)

type substitution = Path.t * Predicate.pexpr

type open_assignment = Top | Bottom

type qvar = Path.t * open_assignment
type refexpr = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type recref = refinement option list list

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

(**************************************************************)
(***************** Type definitions: Frames *******************)
(**************************************************************)

type t =
  | Fvar of Path.t * refinement
  | Frec of Path.t * recref
  | Fsum of Path.t * (Path.t * recref) option * constr list * refinement
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
  | (Funknown | Fvar _ | Frec _) as fr -> f fr
  | Fsum (p, ro, cs, r) ->
      f (Fsum (p, ro, List.map (fun (cd, ps) -> (cd, map_params f ps)) cs, r))
  | Fabstract (p, ps, r) -> f (Fabstract (p, map_params f ps, r))
  | Farrow (x, f1, f2) -> f (Farrow (x, map f f1, map f f2))

let map_recref f rr =
  List.map (fun r -> List.map (M.may_map f) r) rr

let rec map_refinements_map f = function
  | Fvar (p, r) -> Fvar (p, f r)
  | Frec (p, rr) -> Frec (p, map_recref f rr)
  | Fsum (p, ro, cs, r) -> Fsum (p, M.may_map (fun (p, rr) -> (p, map_recref f rr)) ro, cs, f r)
  | Fabstract (p, ps, r) -> Fabstract (p, ps, f r)
  | f -> f

let map_refinements f fr =
  map (map_refinements_map f) fr

let map_refexprs f fr =
  map_refinements (fun r -> List.map f r) fr

let recref_fl f r l = match r with
  | Some r -> f r l
  | None   -> l

let recref_fold f rr l =
  List.fold_right (recref_fl f) (List.flatten rr) l

let rec refinement_fold f l = function
  | Fvar (_, r) -> f r l
  | Frec (_, rr) -> recref_fold f rr l
  | Fsum (_, ro, cs, r) ->
      f r (List.fold_left (refinement_fold f) (match ro with Some (_, rr) -> recref_fold f rr l | None -> l) (constrs_param_frames cs))
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

let refinement_is_empty qes =
  List.for_all (function (_, ([], [])) -> true | _ -> false) qes

let empty_recref cs =
  List.map (fun (_, ps) -> List.map (function Frec _ -> None | _ -> Some empty_refinement) (params_frames ps)) cs

let row_is_empty rs =
  List.for_all (function None -> true | Some r -> refinement_is_empty r) rs

let recref_is_empty rr =
  List.for_all row_is_empty rr

let false_refinement =
  mk_refinement [] [(Path.mk_ident "false", Path.mk_ident "V", Predicate.Not (Predicate.True))] []

let apply_refinement r = function
  | Fvar (p, _) -> Fvar (p, r)
  | Fsum (p, rr, cs, _) -> Fsum (p, rr, cs, r)
  | Fabstract (p, ps, _) -> Fabstract (p, ps, r)
  | f -> f

let get_refinement = function
  | Fvar (_, r) | Fsum (_, _, _, r) | Fabstract (_, _, r) -> Some r
  | _ -> None

let append_refinement res' f =
  match get_refinement f with
    | Some res -> apply_refinement (res @ res') f
    | None -> f

let apply_recref rr = function
  | Frec (p, _)                   -> Frec (p, rr)
  | Fsum (p, Some (rp, _), cs, r) -> Fsum (p, Some (rp, rr), cs, r)
  | _                             -> assert false

let get_recref = function
  | Frec (_, rr) | Fsum (_, Some (_, rr), _, _) -> Some rr
  | _                                           -> None

let merge_recrefs rr rr' =
  let merge r r' = match (r, r') with
    | (Some r, Some r') -> Some (r @ r')
    | (None, None)      -> None
    | _                 -> assert false
  in List.map2 (List.map2 merge) rr rr'

let append_recref rr' f = match get_recref f with
  | Some rr -> apply_recref (merge_recrefs rr rr') f
  | None    -> f

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

let shape f =
  map_refinements (fun _ -> empty_refinement) f

let same_shape t1 t2 =
  let vars = ref [] in
  let ismapped p q = try snd (List.find (fun (p', q) -> Path.same p p') !vars) = q with
      Not_found -> vars := (p, q) :: !vars; true in
  let rec sshape = function
      (Fsum(p, ro, cs, _), Fsum(p', ro', cs', _)) ->
        Path.same p p' && params_sshape (constrs_params cs) (constrs_params cs') &&
          (ro = ro' || match (ro, ro') with (Some (rp, _), Some (rp', _)) -> ismapped rp rp' | _ -> false)
    | (Fabstract(p, ps, _), Fabstract(p', ps', _)) ->
        Path.same p p' && params_sshape ps ps'
    | (Fvar (p, _), Fvar (p', _)) | (Frec (p, _), Frec (p', _)) ->
        ismapped p p'
    | (Farrow(_, i, o), Farrow(_, i', o')) ->
        sshape (i, i') && sshape (o, o')
    | (Funknown, Funknown) -> true
    | t -> false
  and params_sshape ps qs =
    List.for_all sshape (List.combine (params_frames ps) (params_frames qs))
  in sshape (t1, t2)

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

let comma ppf =
  fprintf ppf ",@;<1 0>"

let pprint_refs ppf rs =
  fprintf ppf "@[[%a]@]" (Oprint.print_list (fun ppf -> function Some r -> pprint_refinement ppf r | None -> fprintf ppf "None") comma) rs

let pprint_recref ppf rr =
  fprintf ppf "@[[%a]@]" (Oprint.print_list pprint_refs comma) rr

let pprint_recopt ppf = function
  | Some (rp, rr) -> fprintf ppf "%a@;<1 0>μ%s." pprint_recref rr (C.path_name () rp)
  | None          -> fprintf ppf "" (* pmr: necessary? *)

let rec pprint ppf = function
  | Fvar (a, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "'%s" (C.path_name () a)) r
  | Frec (path, rr) ->
      fprintf ppf "@[%a %s@]" pprint_recref rr (C.path_name () path)
  | Fsum (path, ro, [], r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a%s@]" pprint_recopt ro (C.path_name () path)) r
  | Fsum (path, ro, cs, r) ->
      wrap_refined ppf (fun ppf -> fprintf ppf "@[%a %s @[(%a)@]@]"
                          pprint_recopt ro
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

(******************************************************************************)
(********************************** Unfolding *********************************)
(******************************************************************************)

let rec apply_refs (rs : refinement option list) ps = match (rs, ps) with
  | (r :: rs, (i, f, v) :: ps) ->
      let (ip, i') = (Path.Pident i, Ident.create (Ident.name i)) in
      let sub      = (ip, Predicate.Var (Path.Pident i')) in
      let ps       = List.map (fun (i, f, v) -> (i, apply_substitution sub f, v)) ps in
      let rs       = List.map (Misc.may_map (List.map (apply_substitution_map sub))) rs in
        (i', (match r with Some r -> append_refinement r f | None -> f), v) :: apply_refs rs ps
  | ([], []) -> []
  | _        -> assert false

let apply_recref_constrs rr cs =
  List.map2 (fun rs (t, ps) -> (t, apply_refs rs ps)) rr cs

let apply_recref rr = function
  | Fsum (p, ro, cs, r) -> Fsum (p, ro, apply_recref_constrs rr cs, r)
  | _                   -> assert false

let unfold_with f f' = match f with
  | Fsum (p, Some (rp, rr), cs, r) ->
      map (function Frec (rp', rr') when Path.same rp rp' -> append_recref rr' f' | f -> f) (Fsum (p, None, cs, r))
  | _ -> f

let unfold f =
  unfold_with f f

(**************************************************************)
(********* Polymorphic and qualifier instantiation ************) 
(**************************************************************)

(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.
   If a variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate fr ftemplate =
  let vars = ref [] in
  let vmap p ft =
    try List.assoc p !vars with Not_found -> vars := (p, ft) :: !vars; ft
  in
  let rec inst f ft =
    match (f, ft) with
      | (Fvar (p, r), _) ->
          let instf = vmap p ft in append_refinement r instf
      | (Frec (p, _), _) ->
          vmap p ft
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
      | (Fsum (p, _, cs, r), Fsum(p', ro', cs', _)) ->
          Fsum(p, ro', List.map2 (fun (cd, ps) (_, ps') -> (cd, inst_params ps ps')) cs cs', r)
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
    | (Fvar _, Fvar _) | (Frec _, Frec _) | (Funknown, Funknown)
    | (Fsum _, Fsum _) | (Fabstract _, Fabstract _) ->
        instantiate_qualifiers vars f
    | (Farrow (None, f1, f1'), Farrow (l, f2, f2')) ->
        Farrow (l, label vars f1 f2, label vars f1' f2')
    | (Farrow (Some p1, f1, f1'), Farrow (Some p2, f2, f2')) ->
        let vars' = List.map (fun (x, y) -> (Ident.name x, Path.Pident y)) (Pattern.bind_vars p1 p2) @ vars in
          Farrow (Some p2, label vars f1 f2, label vars' f1' f2')
    | _ -> printf "Can't label %a like %a" pprint f pprint f'; assert false
  in label [] f f'

let record_of_params path ps r =
  Fsum(path, None, [(Cstr_constant 0, ps)], r)

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
          in Fsum (p, None, List.map fresh_cstr cds, empty_refinement)

let close_recf rv = function
  | Fsum (p, _, cs, r) ->
      let f = Fsum (p, Some (rv, empty_recref cs), cs, r) in if unfold f = f then Fsum (p, None, cs, r) else f
  | f -> f

let fresh_rec fresh env rv t = match t.desc with
  | Tvar                 -> fresh_fvar ()
  | Tarrow(_, t1, t2, _) -> Farrow (None, fresh [] t1, fresh [] t2)
  | Ttuple ts            -> tuple_of_frames (List.map (fresh []) ts) empty_refinement
  | Tconstr(p, tyl, _)   -> close_recf rv (fresh_constr fresh env p t (List.map canonicalize tyl))
  | _                    -> fprintf err_formatter "@[Warning: Freshing unsupported type]@."; Funknown

let cstr_refinements cstr =
  let (args, res) = Ctype.instance_constructor cstr in
  let _           = close_constructor res args in
  let (res, args) = (canonicalize res, List.map canonicalize args) in
    List.map (fun t -> if t = res then None else Some empty_refinement) args

let mk_recvar env t =
  let rv = Path.mk_ident "t" in
    match t.desc with
      | Tconstr (p, tyl, _) ->
          let (_, cstrs) = List.split (Env.constructors_of_type p (Env.find_type p env)) in
          let rr         = List.map cstr_refinements cstrs in (rv, Frec (rv, rr))
      | _ -> (rv, Frec (rv, []))

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
          let (rv, rf) = mk_recvar env t in
            Hashtbl.replace tbl t rf;
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
    | (Tpat_tuple pats, Fsum (_, _, [(_, ps)], _)) ->
        (List.combine (Pattern.pattern_descs pats) (params_frames ps), [])
    | (Tpat_construct (cstrdesc, pats), Fsum (p, _, cfvs, _)) ->
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
  | Fvar(_, r) | Fsum(_, _, _, r) | Fabstract(_, _, r) ->
      refinement_conjuncts solution qual_expr r @ cs
  | _ -> cs

let rec conjuncts solution qual_expr fr =
  conjunct_fold [] solution qual_expr fr

let predicate solution qual_expr f =
  Predicate.big_and (conjuncts solution qual_expr f)
