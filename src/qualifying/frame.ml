open Types
open Typedtree
open Btype
open Format
open Asttypes

type substitution = Path.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Path.t                      (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type t =
    Fvar of Path.t
  | Fconstr of Path.t * t list * refinement
  | Farrow of pattern_desc option * t * t
  | Ftuple of t list
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Path.name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pprint_refinement ppf refi =
  match refi with
    | (_, Qvar id ) -> fprintf ppf "%s" (Path.name id)
    | (subs, Qconst quals) ->
      let preds = List.map (Qualifier.apply (Path.mk_ident "V")) quals in
      let preds = List.map (Predicate.apply_substs subs) preds in
        Oprint.print_list Predicate.pprint (fun ppf -> fprintf ppf "@ ") ppf preds

let rec pprint_pattern ppf = function
  | Tpat_any -> fprintf ppf "_"
  | Tpat_var x -> fprintf ppf "%s" (Ident.name x)
  | Tpat_tuple pats ->
    let pats = List.map (fun p -> p.pat_desc) pats in
      fprintf ppf "(%a)" (Oprint.print_list pprint_pattern (fun pff -> fprintf ppf ", ")) pats
  | _ -> assert false

let rec pprint ppf = function
  | Fvar a ->
      fprintf ppf "Var(%s)" (Path.unique_name a)
  | Fconstr (path, [], r) ->
      fprintf ppf "@[{%s@ |@;<1 2>%a}@]" (Path.name path) pprint_refinement r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some pat, f, f') ->
      fprintf ppf "@[%a:@ %a@ ->@;<1 2>%a@]" pprint_pattern pat pprint1 f pprint f'
  | Fconstr (path, l, r) ->
      fprintf ppf "@[{%a@ %s|@;<1 2>%a}@]" pprint (List.hd l) (Path.unique_name path) pprint_refinement r
   | Ftuple ts ->
      fprintf ppf "@[(%a)@]" (Oprint.print_list pprint (fun ppf -> fprintf ppf ",@;<1 2>")) ts
   | Frecord (id, _, r) ->
       fprintf ppf "@[{%s |@;<1 2>%a}@] " (Path.name id) pprint_refinement r
  | Funknown ->
      fprintf ppf "[unknown]"
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f

let empty_refinement = ([], Qconst [])

let fresh_refinementvar () = ([], Qvar (Path.mk_ident "k"))

let fresh_fvar () = Fvar (Path.mk_ident "a")

(* Create a fresh frame with the same shape as the type of [exp] using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun exp fresh_ref_var =
  let vars = ref [] in
  let (ty, env) = (repr exp.exp_type, exp.exp_env) in
  let rec fresh_rec t =
    let t' = repr t in
    match t'.desc with
        Tvar ->
          begin try List.assq t' !vars
          with Not_found ->
            let fv = fresh_fvar () in
              vars := (t', fv) :: !vars;
              fv
          end
       (* ming: there are some types that we'd like to essentially leave unrefined
          -- for readability and generally being careful we'd like these refined as true,
          but that would require special casing the case below, which is inelegant. *)
       (* pmr: badness; should be handled just fine by well-formedness constraints,
          which would effectively effect the same effect *)
      | Tconstr(p, tyl, _) ->
          let ty_decl = Env.find_type p env in
            begin match ty_decl.type_kind with
              | Type_abstract
              | Type_variant _ ->
                  if Path.same p Predef.path_unit || Path.name p = "garbage" then
                    Fconstr (p, [], ([], Qconst []))
                  else
                    Fconstr (p, List.map fresh_rec tyl, fresh_ref_var ())
              | Type_record (fields, _, _) ->
                  (* Tedium ahead:
                     OCaml stores information about record types in two places:
                     - The type declaration stores everything that's set in stone about the
                       type: what its fields are and which variables are the type parameters.
                     - The type list of the Tconstr contains the actual instantiations of
                       the tyvars in this instantiation of the record. *)
                  let param_map = List.map2 (fun tyvar tyinst -> (tyvar, tyinst)) ty_decl.type_params tyl in
                  let fresh_field (name, muta, typ) =
                    let field_typ = try List.assoc typ param_map with Not_found -> typ in
                      (fresh_rec field_typ, name, muta)
                  in Frecord (p, List.map fresh_field fields, fresh_ref_var ())
          end
      | Tarrow(_, t1, t2, _) ->
          Farrow (None, fresh_rec t1, fresh_rec t2)
      | Ttuple ts ->
          Ftuple (List.map fresh_rec ts)
      | _ ->
          fprintf err_formatter "@[Warning:@ Freshing@ unsupported@ type@]@.";
          Funknown
  in fresh_rec ty

(* Create a fresh frame with the same shape as the given type
   [ty]. Uses type environment [env] to find type declarations.

   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh exp = fresh_with_var_fun exp fresh_refinementvar

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars exp = fresh_with_var_fun exp (fun _ -> empty_refinement)

(* Label all the function formals in [f] with their corresponding labels in
   [f'].  Obviously, they are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let rec label_like f f' =
  match (f, f') with
    | (Fvar _, Fvar _)
    | (Fconstr _, Fconstr _)
    | (Funknown, Funknown) -> f
    | (Farrow (None, f1, f1'), Farrow(l, f2, f2')) ->
        Farrow (l, label_like f1 f2, label_like f1' f2')
    | (Ftuple t1s, Ftuple t2s) ->
        Ftuple (List.map2 label_like t1s t2s)
    | (Frecord (p1, f1s, r), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
        let label_rec (f1, n, muta) (f2, _, _) = (label_like f1 f2, n, muta) in
          Frecord (p1, List.map2 label_rec f1s f2s, r)
    | _ -> assert false

(* Create a fresh frame with the same shape as [exp]'s type and [f],
   and the same labels as [f]. *)
let fresh_with_labels exp f = label_like (fresh exp) f

(* Instantiate the tyvars in fr with the corresponding frames in ftemplate.  If a
   variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let instantiate fr ftemplate =
  let vars = ref [] in
  let rec inst f ft =
    match (f, ft) with
      | (Fvar _, _) ->
          begin try List.assq f !vars
          with Not_found ->
            vars := (f, ft) :: !vars;
            ft
          end
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
      | (Fconstr (p, l, r), Fconstr(p', l', _)) ->
	    Fconstr(p, List.map2 inst l l', r)
      | (Ftuple t1s, Ftuple t2s) ->
          Ftuple (List.map2 inst t1s t2s)
      | (Frecord (p, f1s, r), Frecord (_, f2s, _)) ->
          let inst_rec (f1, name, m) (f2, _, _) = (inst f1 f2, name, m) in
            Frecord (p, List.map2 inst_rec f1s f2s, r)
      | (Funknown, Funknown) -> Funknown
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false
  in inst fr ftemplate

(* Apply a substitution to a frame, distributing over arrows. *)
let rec apply_substitution sub = function
  | (Fvar _) as f -> f
  | Fconstr (p, [], (subs, qe)) ->
      Fconstr (p, [], (sub :: subs, qe))
  | Farrow (x, f1, f2) ->
      Farrow (x, apply_substitution sub f1, apply_substitution sub f2)
  | Fconstr (p, l, (subs, qe)) ->
     Fconstr (p, List.map (apply_substitution sub) l, (sub::subs, qe))
  | Ftuple ts ->
      Ftuple (List.map (apply_substitution sub) ts)
  | Frecord (p, fs, (subs, qe)) ->
      let apply_rec (f, n, m) = (apply_substitution sub f, n, m) in
        Frecord (p, List.map apply_rec fs, (sub :: subs, qe))
  | Funknown -> Funknown

let refinement_apply_solution solution = function
  | (subs, Qvar k) -> (subs, Qconst (solution k))
  | r -> r

let apply_solution solution fr =
  let rec apply_rec = function
    | Farrow (x, f, f') ->
        Farrow (x, apply_rec f, apply_rec f')
    | Fconstr (path, fl, r) ->
        Fconstr (path, List.map apply_rec fl, refinement_apply_solution solution r)
    | Ftuple ts ->
        Ftuple (List.map apply_rec ts)
    | Frecord (p, fs, r) ->
        Frecord (p, List.map (fun (f, n, m) -> (apply_rec f, n, m)) fs, refinement_apply_solution solution r)
    | Fvar _
    | Funknown as f-> f
  in apply_rec fr

let refinement_conjuncts solution qual_var (subs, qualifiers) =
  let quals = match qualifiers with
    | Qvar k -> (try solution k with Not_found -> assert false)
    | Qconst qs -> qs
  in
  let unsubst = List.map (Qualifier.apply qual_var) quals in
    List.map (Predicate.apply_substs subs) unsubst

let refinement_predicate solution qual_var refn =
  Predicate.big_and (refinement_conjuncts solution qual_var refn)

let rec refinement_vars = function
  | Fconstr (_, _, (_, Qvar k)) -> [k]
  | Frecord (_, fs, (_, Qvar k)) ->
      k :: List.fold_left (fun r (f, _, _) -> refinement_vars f @ r) [] fs
  | _ -> []

let apply_refinement r = function
  | Fconstr (p, fl, _) -> Fconstr (p, fl, r)
  | Frecord (p, fs, _) -> Frecord (p, fs, r)
  | f -> f

(* pmr: sound for our uses but not very informative *)
let rec conjuncts solution qual_var = function
  | Fconstr (_, _, r) -> refinement_conjuncts solution qual_var r
  | _ -> []

let rec predicate solution qual_var = function
    Fconstr(_, _, r) ->
      refinement_predicate solution qual_var r
      (* pmr: need to embed on constructed types, much like below *)
  | Frecord (p, fs, r) ->
      let make_subframe_pred (f, name, _) =
        let pred = predicate solution qual_var f in
          Predicate.subst (Predicate.Field (name, Predicate.Var qual_var)) qual_var pred
      in Predicate.big_and (refinement_predicate solution qual_var r :: List.map make_subframe_pred fs)
  | _ -> Predicate.True
