open Types
open Btype
open Format

type substitution = Ident.t * Predicate.pexpr

type qualifier_expr =
    Qvar of Ident.t                     (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type t =
    Fvar of Ident.t
  | Fconstr of Path.t * t list * refinement
  | Farrow of Ident.t option * t * t

let pprint_qualifier_expr ppf = function
  | Qvar id ->
      fprintf ppf "%s" (Ident.unique_name id)
  | Qconst quals ->
      Oprint.print_list Qualifier.pprint (fun ppf -> fprintf ppf "@ ") ppf quals

let pprint_sub ppf (id, pexp) =
  fprintf ppf "@[%s@ ->@ %a@]" (Ident.unique_name id) Predicate.pprint_pexpr pexp

let pprint_subs ppf subs =
  Oprint.print_list pprint_sub (fun ppf -> fprintf ppf ";@ ") ppf subs

let pprint_refinement ppf (subs, qexp) =
  fprintf ppf "@[[%a]@;<1 2>%a@]" pprint_subs subs pprint_qualifier_expr qexp

let rec pprint ppf = function
  | Fvar a ->
      fprintf ppf "%s" (Ident.unique_name a)
  | Fconstr (path, [], r) ->
      fprintf ppf "@[{%s@ |@;<1 2>%a}@]" (Path.name path) pprint_refinement r
  | Farrow (None, f, f') ->
      fprintf ppf "@[%a@ ->@;<1 2>%a@]" pprint1 f pprint f'
  | Farrow (Some id, f, f') ->
      fprintf ppf "@[%s:@ %a@ ->@;<1 2>%a@]" (Ident.unique_name id) pprint1 f pprint f'
  | _ -> assert false
 and pprint1 ppf = function
   | (Farrow _) as f ->
       fprintf ppf "@[(%a)@]" pprint f
   | _ as f -> pprint ppf f

let fresh_refinementvar () = ([], Qvar (Ident.create "k"))

(* Create a fresh frame with the same shape as the given type [ty].
   You probably want to consider using fresh_with_labels instead of this
   for subtype constraints. *)
let fresh ty =
  let vars = ref [] in
  let rec fresh_rec t =
    let t' = repr t in
    match t'.desc with
        Tvar ->
          begin try List.assq t' !vars
          with Not_found ->
            let fv = Fvar (Ident.create "a") in
              vars := (t', fv) :: !vars;
              fv
          end
      | Tconstr(p, tyl, _) ->
          Fconstr (p, List.map fresh_rec tyl, fresh_refinementvar ())
      | Tarrow(_, t1, t2, _) ->
          Farrow (None, fresh_rec t1, fresh_rec t2)
      | _ -> assert false
  in fresh_rec ty

(* Instantiate the vars in f with the corresponding frames in ftemplate.  If a
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
      | (Fconstr (p, [], r), Fconstr (_, [], _)) ->
          Fconstr (p, [], r)
      | (Farrow (l, f1, f1'), Farrow (_, f2, f2')) ->
          Farrow (l, inst f1 f2, inst f1' f2')
      | _ -> assert false
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
  | _ -> assert false

(* Label all the function formals in [f] with their corresponding labels in
   [f'].  Obviously, they are expected to be of the same shape; also, [f]
   must be completely unlabeled (as frames are after creation by fresh). *)
let rec label_like f f' =
  match (f, f') with
    | (Fvar _, Fvar _)
    | (Fconstr _, Fconstr _) ->
        f
    | (Farrow (None, f1, f1'), Farrow(l, f2, f2')) ->
        Farrow (l, label_like f1 f2, label_like f1' f2')
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
        Fconstr (path, List.map apply_rec fl,
                 refinement_apply_solution solution r)
    | (Fvar _) as f -> f
  in apply_rec fr

let refinement_predicate solution qual_var (subs, qualifiers) =
  let quals = match qualifiers with
    | Qvar k -> Lightenv.find k solution
    | Qconst qs -> qs
  in
  let unsubst = Predicate.big_and (List.map (Qualifier.apply qual_var) quals) in
  let substitute p (x, e) = Predicate.subst e x p in
    List.fold_left substitute unsubst subs

let predicate solution qual_var = function
    Fconstr(_, _, r) ->
      refinement_predicate solution qual_var r
      (* pmr: need to elementify on constructed types *)
  | _ -> Predicate.True

let refinement_well_formed env solution r =
  let valu = Ident.create "valu" in
  let var_bound v = v = valu or Lightenv.mem v env in
    List.for_all var_bound
      (Predicate.vars (refinement_predicate solution valu r))
