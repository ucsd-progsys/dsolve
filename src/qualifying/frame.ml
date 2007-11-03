open Types
open Btype
open Format

type substitution = Path.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Path.t                      (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type t =
    Fvar of Path.t
  | Fconstr of Path.t * t list * refinement
  | Farrow of Path.t option * t * t
  | Ftuple of t * t
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
      fprintf ppf "@[{%s@ |@;<1 2>%a}@]" (Path.name path) pprint_refinement r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some id, f, f') ->
      fprintf ppf "@[%s:@ %a@ ->@;<1 2>%a@]" (Path.unique_name id) pprint1 f pprint f'
  | Fconstr (path, l, r) ->
      fprintf ppf "@[{%a@ %s|@;<1 2>%a}@]" pprint (List.hd l) (Path.unique_name path) pprint_refinement r
   | Ftuple(t1, t2) ->
      fprintf ppf "@[(%a,@ %a)@]" pprint t1 pprint t2
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
let frame_bool = frame_cons Predef.path_bool

let fresh_refinementvar () = ([], Qvar (Path.mk_ident "k"))

let fresh_fvar () = Fvar (Path.mk_ident "a")

(* ming: abbrevs? *)
(* Create a fresh frame with the same shape as the given type [ty], using
   [fresh_ref_var] to create new refinement variables. *)
let fresh_with_var_fun ty fresh_ref_var =
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
          if Path.same p Predef.path_unit then
            Fconstr (p, [], ([], Qconst []))
          else
            Fconstr (p, List.map fresh_rec tyl, fresh_ref_var ())
      | Tarrow(_, t1, t2, _) ->
          Farrow (None, fresh_rec t1, fresh_rec t2)
      (* ming: placeholder for full tuples *)
      | Ttuple(ts) ->
          Ftuple (fresh_rec (List.hd ts), fresh_rec (List.hd (List.tl ts)))
      | _ ->
          fprintf err_formatter "@[Warning:@ Freshing@ unsupported@ type@]@.";
          Funknown
  in fresh_rec ty

let rec type_structure t =
  let t' = repr t in
  match t'.desc with
        Tvar ->
          Printf.sprintf "Tvar" 
      | Tconstr(p, tyl, _) -> 
          Printf.sprintf "Tconstr: path(%s) types: (%s)" (Path.name p) 
                        (String.concat " " (List.map type_structure tyl))
      | Tarrow(p, t1, t2, _) ->
          Printf.sprintf "Tarrow: path(%s) type_in: (%s) type_out: (%s)" (p)
            (type_structure t1) (type_structure t2)
      | Ttuple(ts) ->
          Printf.sprintf "Ttuple: types: (%s)" 
                        (String.concat " " (List.map type_structure ts))
      | Tobject(_, _) ->
          Printf.sprintf "Tobject"
      | Tfield(_, _, _, _) ->
          Printf.sprintf "Tfield"
      | Tnil ->
          Printf.sprintf "Tnil"
      | Tlink(t) ->
          Printf.sprintf "Tlink: %s" (type_structure t)
      | Tsubst(_) ->
          Printf.sprintf "Tsubst"
      | Tvariant(_) ->
          Printf.sprintf "Tvariant"
      | Tunivar ->
          Printf.sprintf "Tunivar"
      | Tpoly(_, _) ->
          Printf.sprintf "Tpoly"

(* Create a fresh frame with the same shape as the given type [ty].
   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh ty = fresh_with_var_fun ty fresh_refinementvar

(* Create a fresh frame with the same shape as the given type [ty].
   No refinement variables are created - all refinements are initialized
   to true. *)
let fresh_without_vars ty = fresh_with_var_fun ty (fun _ -> empty_refinement)

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
        (* this happens in (hopefully) exactly one case *)
      (*| (Fconstr(p, [], r), Fvar(_)) ->
          Fconstr(p, [], r) *)
      | (Fconstr (p, [], r), Fconstr (_, [], _)) ->
          Fconstr (p, [], r)
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
			| (Fconstr (p, l, r), Fconstr(p', l', r')) ->
					(*let _ = if Path.same p p' then () else assert false in*)
					(*let _ = if r = r' then () else assert false in*)
					Fconstr(p, List.map2 inst l l', r)
      | (Ftuple(t1, t2), Ftuple(t1', t2')) ->
          Ftuple(inst t1 t1', inst t2 t2')
      | (Funknown, Funknown) -> Funknown
      | (f1, f2) ->
          fprintf std_formatter "@[Unsupported@ types@ for@ instantiation:@;<1 2>%a@;<1 2>%a@]@."
	    pprint f1 pprint f2;
	    assert false
      (*| _ -> assert false*)
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
  | Ftuple(t1, t2) ->
      Ftuple(apply_substitution sub t1, apply_substitution sub t2)
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
    | (Ftuple(t1, t2), Ftuple(t1', t2')) ->
        Ftuple(label_like t1 t1', label_like t2 t2')
    | _ -> assert false

(* Create a fresh frame with the same shape as [t] and [f],
   and the same labels as [f]. *)
let fresh_with_labels t f =
  label_like (fresh t) f

let refinement_apply_solution solution = function
    (subs, Qvar k) -> (subs, Qconst (Lightenv.find k solution))
  | r -> r

let apply_solution solution fr =
  let rec apply_rec = function
    | Farrow (x, f, f') ->
        Farrow (x, apply_rec f, apply_rec f')
    | Fconstr (path, fl, r) ->
        Fconstr (path, List.map apply_rec fl, refinement_apply_solution solution r)
    | Ftuple(t1, t2) ->
        Ftuple(apply_rec t1, apply_rec t2)
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

let predicate solution qual_var = function
    Fconstr(_, _, r) ->
      refinement_predicate solution qual_var r
      (* pmr: need to elementify on constructed types *)
  | _ -> Predicate.True

let rec same_shape t1 t2 =
  match (t1, t2) with
  (Fconstr(p, l, _), Fconstr(p', l', _)) ->
   (Path.same p p') && (List.for_all (fun f -> f) (List.map2 same_shape l l')) 
  | (Fvar p, Fvar p') ->
   Path.same p p'
  | (Farrow(_, i, o), Farrow(_, i', o')) ->
   (same_shape i i') && (same_shape o o')
  | (Ftuple(f, g), Ftuple(f', g')) ->
   (same_shape f f') && (same_shape g g')
  | (Funknown, Funknown) -> true
  | t -> false

let pred_is_well_typed env p = 
  let rec get_expr_shape = function
  | Predicate.PInt _ -> frame_int
  | Predicate.Var x  
  | Predicate.Pvar (x, _) -> (try Lightenv.find x env
                        with Not_found -> assert false)
  | Predicate.FunApp (s, p') -> 
      (* ming: huge hack alert *)
      if s = "Array.length" then 
        let arg_shp = get_expr_shape p' in
        match arg_shp with
          Fconstr(a, _, _) -> 
            if Path.same Predef.path_array a then 
              frame_int 
            else
              Funknown
          | _ -> Funknown
        else assert false
  | Predicate.Binop (p1, op, p2) ->
      let p1_shp = get_expr_shape p1 in
      let p1_int = same_shape p1_shp frame_int in
      let p2_shp = get_expr_shape p2 in
      let p2_int = same_shape p2_shp frame_int in
      if p1_int && p2_int then frame_int else Funknown
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
         (same_shape p1_shp p2_shp) && not(same_shape p1_shp Funknown)
         || ((same_shape p1_shp frame_bool) && (same_shape p2_shp frame_int))
         || ((same_shape p1_shp frame_int) && (same_shape p2_shp frame_bool))
        | Predicate.Gt
        | Predicate.Ge
        | Predicate.Lt
        | Predicate.Le ->
        (same_shape p1_shp p2_shp) && (same_shape p1_shp frame_int || 
                                       (function Fvar _ -> true | _ -> false) p1_shp) 
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


