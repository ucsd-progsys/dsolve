open Types
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
  | Farrow of Path.t option * t * t
  | Ftuple of t list
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

let pprint_qualifier_expr ppf = function
  | Qvar id ->
      fprintf ppf "%s" (Path.unique_name id)
  | Qconst quals ->
      Oprint.print_list Qualifier.pprint (fun ppf -> fprintf ppf "@ ") ppf quals

let pprint_sub ppf (path, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Path.unique_name path) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pprint_refinement ppf (subs, qexp) =
  fprintf ppf "@[[%a]@;<1 2>%a@]" pprint_subs subs pprint_qualifier_expr qexp

let rec pprint ppf = function
  | Fvar a ->
      fprintf ppf "Var(%s)" (Path.unique_name a)
  | Fconstr (path, [], r) ->
      fprintf ppf "@[{%s@ |@;<1 2>%a}@]" (Path.unique_name path) pprint_refinement r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some id, f, f') ->
      fprintf ppf "@[%s:@ %a@ ->@;<1 2>%a@]" (Path.unique_name id) pprint1 f pprint f'
  | Fconstr (path, l, r) ->
      fprintf ppf "@[{%a@ %s|@;<1 2>%a}@]" pprint (List.hd l) (Path.unique_name path) pprint_refinement r
   | Ftuple ts ->
      fprintf ppf "@[(%a)@]" (Oprint.print_list pprint (fun ppf -> fprintf ppf ",@;<1 2>")) ts
   | Frecord (id, _, r) ->
       fprintf ppf "@[{%s |@;<1 2>%a}@] " (Path.unique_name id) pprint_refinement r
  | Funknown ->
      fprintf ppf "[unknown]"
  (*| _ -> assert false*)
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f

let empty_refinement = ([], Qconst [])

(* ming: note this is a redefinition of builtins.mk_... *)

let frame_cons p = Fconstr(p, [], empty_refinement)
let frame_int = frame_cons Predef.path_int
let frame_float = frame_cons Predef.path_float
let frame_bool = frame_cons Predef.path_bool

let fresh_refinementvar () = ([], Qvar (Path.mk_ident "k"))

let fresh_fvar () = Fvar (Path.mk_ident "a")

(* ming: abbrevs? *)
(* Create a fresh frame with the same shape as the given type [ty], using
   [fresh_ref_var] to create new refinement variables.  Uses type environment
   [env] to find type declarations. *)
let fresh_with_var_fun ty env fresh_ref_var =
  let vars = ref [] in
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
                  if Path.same p Predef.path_unit || Path.same p Predef.path_float then
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
let fresh ty env = fresh_with_var_fun ty env fresh_refinementvar

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars ty env = fresh_with_var_fun ty env (fun _ -> empty_refinement)

(* Instantiate the vars in f(r) with the corresponding frames in ftemplate.  If a
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
	  (*let _ = if Path.same p p' then () else assert false in*)
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

(* Apply a substitution to a frame, distributing over arrows.  Unaliases any
   aliases created by fresh.

   (If it did not do so, we would get bogus backward-substitutions; imagine
    applying a function of type x: k int -> y: z int -> k int) *)
let rec apply_substitution sub = function
    (Fvar _) as f ->
      f
  | Fconstr (p, [], (subs, qe)) ->
      Fconstr (p, [], (sub :: subs, qe))
  | Farrow (x, f1, f2) ->
      Farrow (x, apply_substitution sub f1, apply_substitution sub f2)
  | Fconstr (p, l, (subs, qe)) ->
     (* Fconstr (p, List.map (apply_substitution sub) l, (sub::subs, qe))*)
     (* ming: let's not prop substitutions into compound types and see what
      * happens *)
     Fconstr (p, l, (sub::subs, qe))
  | Ftuple ts ->
      Ftuple (List.map (apply_substitution sub) ts)
  | Frecord (p, fs, (subs, qe)) ->
      let apply_rec (f, n, m) = (apply_substitution sub f, n, m) in
        Frecord (p, List.map apply_rec fs, (sub :: subs, qe))
  | Funknown ->
      Funknown
  (*| _ -> assert false*)

(* Label all the function formals in [f] with their corresponding labels in
   [f'].  Obviously, they are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let rec label_like f f' =
  match (f, f') with
    | (Fvar _, Fvar _)
    | (Fconstr _, Fconstr _)
    | (Funknown, Funknown) ->
        f
    | (Farrow (None, f1, f1'), Farrow(l, f2, f2')) ->
        Farrow (l, label_like f1 f2, label_like f1' f2')
    | (Ftuple t1s, Ftuple t2s) ->
        Ftuple (List.map2 label_like t1s t2s)
    | (Frecord (p1, f1s, r), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
        let label_rec (f1, n, muta) (f2, _, _) = (label_like f1 f2, n, muta) in
          Frecord (p1, List.map2 label_rec f1s f2s, r)
    | _ -> assert false

(* Create a fresh frame with the same shape as [t] and [f],
   and the same labels as [f]. Uses type environment
   [env] to find type declarations. *)
let fresh_with_labels t f env =
  label_like (fresh t env) f

let refinement_apply_solution solution = function
    (subs, Qvar k) -> (subs, Qconst (Lightenv.find k solution))
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

let refinement_predicate solution qual_var (subs, qualifiers) =
  let quals = match qualifiers with
    | Qvar k -> (try Lightenv.find k solution with Not_found -> assert false)
    | Qconst qs -> qs
  in
  let unsubst = Predicate.big_and (List.map (Qualifier.apply qual_var) quals) in
  let substitute p (x, e) = Predicate.subst e x p in
    List.fold_left substitute unsubst subs

let refinement_var = function
  | Fconstr (_, _, (_, Qvar k)) ->
      Some k
  | _ -> None

let apply_refinement r = function
  | Fconstr (p, fl, _) ->
      Fconstr (p, fl, r)
  | Frecord (p, fs, _) ->
      Frecord (p, fs, r)
  | f -> f

let rec predicate solution qual_var = function
    Fconstr(_, _, r) ->
      refinement_predicate solution qual_var r
      (* pmr: need to elementify on constructed types, much like below *)
  | Frecord (p, fs, r) ->
      let make_subframe_pred (f, name, _) i =
        let pred = predicate solution qual_var f in
          Predicate.subst (Predicate.Field (name, Predicate.Var qual_var)) qual_var pred
      in          
        Predicate.big_and (refinement_predicate solution qual_var r :: Misc.mapi make_subframe_pred fs)
  | _ -> Predicate.True

let rec same_shape t1 t2 =
  match (t1, t2) with
  (Fconstr(p, l, _), Fconstr(p', l', _)) ->
   (Path.same p p') && (List.for_all (fun f -> f) (List.map2 same_shape l l')) 
  | (Fvar p, Fvar p') ->
   Path.same p p'
  | (Farrow(_, i, o), Farrow(_, i', o')) ->
   (same_shape i i') && (same_shape o o')
  | (Ftuple t1s, Ftuple t2s) ->
   List.for_all2 same_shape t1s t2s
  | (Frecord (p1, f1s, _), Frecord (p2, f2s, _)) when Path.same p1 p2 ->
      let shape_rec (f1, _, _) (f2, _, _) = same_shape f1 f2 in
        List.for_all2 shape_rec f1s f2s
  | (Funknown, Funknown) -> true
  | t -> false

let pred_is_well_typed env p = 
  let rec get_expr_shape = function
  | Predicate.PInt _ -> frame_int
  | Predicate.Var x  
  | Predicate.Pvar (x, _) -> (try Lightenv.find x env
                        with Not_found -> assert false)
  | Predicate.FunApp (s, p') -> 
      let arg_shape shp out_shape =
          match get_expr_shape p' with
              Fconstr(a, _, _) ->
                if Path.same shp a then
                  out_shape
                else
                  Funknown
            | _ -> Funknown
      in
      (* ming: huge hack alert *)
      if s = "Array.length" then arg_shape Predef.path_array frame_int
      else if s = "Bigarray.Array2.dim1" || s = "Bigarray.Array2.dim2" then
        (* pmr: I'm not even going to bother right now *)
        (* ming: hence the huge hack *)
        arg_shape (Builtins.ext_find_type_path "array2") frame_int
      else frame_int (* pmr: yes, I am wickedly subverting this test on purpose *)
  | Predicate.Binop (p1, op, p2) ->
      let p1_shp = get_expr_shape p1 in
      let p1_int = same_shape p1_shp frame_int in
      let p2_shp = get_expr_shape p2 in
      let p2_int = same_shape p2_shp frame_int in
      if p1_int && p2_int then frame_int else Funknown
  | Predicate.Field (f, r) ->
      frame_int (* pmr: obvious scaffolding for the moment *)
  and pred_shape_is_bool = function
  | Predicate.True -> true
  | Predicate.Not p -> pred_shape_is_bool p 
  | Predicate.Or (p1, p2)  
  | Predicate.And (p1, p2) -> (pred_shape_is_bool p1) && (pred_shape_is_bool p2)
  | Predicate.Atom (p1, rel, p2) -> 
      let p1_shp = get_expr_shape p1 in
      let p2_shp = get_expr_shape p2 in
        match rel with
        | Predicate.Ne
        | Predicate.Eq ->
         ((same_shape p1_shp p2_shp) && not(same_shape p1_shp Funknown))
         || ((same_shape p1_shp frame_bool) && (same_shape p2_shp frame_int))
         || ((same_shape p1_shp frame_int) && (same_shape p2_shp frame_bool))
        | Predicate.Gt
        | Predicate.Ge
        | Predicate.Lt
        | Predicate.Le ->
        (same_shape p1_shp p2_shp) && (same_shape p1_shp frame_int || 
                                       (function Fvar _ -> true | _ -> false) p1_shp ||
                                       same_shape p1_shp frame_float) 
    in
      pred_shape_is_bool p

let refinement_well_formed env solution r qual_var =
  let valu = qual_var in
  let pred = refinement_predicate solution valu r in
  let vars = Predicate.vars pred in
  let in_scope =
    let var_bound v = Lightenv.mem v env in
    List.for_all var_bound vars
  in
  if in_scope then pred_is_well_typed env pred else false 


