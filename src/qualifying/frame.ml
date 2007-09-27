open Predicate
open Types
open Btype

type substitution = Ident.t * pexpr

(* Each qualifier variable carries a list of the qualifiers that may be assigned
   to it.  There are two reasons to do this:

   1) It's easiest to specify qualifiers in an "open" way.  That is, to use
   variables that don't necessarily appear in the current context.  For example:

   qualifier GreaterThanA(x): x > a

   let a = ... in let b = a + 1 in ...

   When we create the qualifier variable for b, we can notice that the name a is
   bound in this scope and instantiate the qualifier with the actual identifier
   (Ident.t) for a.

   This solves the problem of what happens when a function, closed over an
   environment containing the variable y, with type ... -> {v | v > y}, is
   called in an environment which also contains the variable y.  Because the
   qualifier on the return type and the qualifiers used on any variables in the
   calling context will use different identifiers for y, there is no chance for
   confusion.

   2) We automatically carry a list of all well-formed qualifiers around with
   each variable, simplifying the previously-quite-ugly refine.
*)
type qualifier_expr =
    Qvar of Qualifier.t list * Ident.t  (* Qualifier variable with list of assignable
                                           qualifiers *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

type frame_desc =
    Fvar
  | Fconstr of Path.t * frame_expr list * refinement
  | Farrow of Ident.t * frame_expr * frame_expr

and frame_expr = frame_desc ref

let fresh_refinementvar env quals =
  let names_to_idents =
    Lightenv.maplist (fun id _ -> (Ident.name id, id)) env in
  let instantiate q =
    try Some (Qualifier.instantiate names_to_idents q)
    with Not_found -> None
  in
  let instantiated_quals = Misc.map_filter instantiate quals in
    ([], Qvar (instantiated_quals, Ident.create "k"))

(* Create a fresh frame with the same shape as the given type.  Identical tyvars
   become references to the same FVar.  (This makes instantiation much
   easier.)

   The environment and qualifiers are used to give each fresh refinement
   variable its own list of well-formed and instantiated qualifiers.
   (See above.)
*)
let fresh env quals ty =
  let vars = ref [] in
  let rec fresh_rec t =
    let t' = repr t in
    match t'.desc with
        Tvar ->
          begin try List.assq t' !vars
          with Not_found ->
            let fv = ref Fvar in
              vars := (t', fv)::!vars;
              fv
          end
      | Tconstr(p, tyl, _) ->
          ref (Fconstr(p, List.map fresh_rec tyl,
                       fresh_refinementvar env quals))
      | Tarrow(_, t1, t2, _) ->
          (* pmr: obviously b0rked *)
          ref (Farrow(Ident.create "", fresh_rec t1, fresh_rec t2))
      | _ -> assert false
  in fresh_rec ty

(* Instantiate the vars in f with the corresponding frames in ftemplate.  If a
   variable occurs twice, it will only be instantiated with one frame; which
   one is undefined and unimportant. *)
let rec instantiate f ftemplate =
  match (!f, !ftemplate) with
      (Fvar, fd) ->
        f := fd
    | (Fconstr(_, fl1, _), Fconstr(_, fl2, _)) ->
        List.iter2 instantiate fl1 fl2
    | (Farrow(_, f1, f1'), Farrow(_, f2, f2')) ->
        instantiate f1 f2;
        instantiate f1' f2'
    | _ -> assert false

(* Apply a substitution to a frame, distributing over arrows.  Unaliases any
   aliases created by fresh.

   (If it did not do so, we would get bogus backward-substitutions; imagine
    applying a function of type x: k int -> y: z int -> k int) *)
let rec apply_substitution sub f =
  match !f with
      Fvar ->
        f
    | Fconstr(p, [], (subs, qe)) ->
        ref (Fconstr(p, [], (sub::subs, qe)))
    | Farrow(x, f1, f2) ->
        ref (Farrow(x, apply_substitution sub f1, apply_substitution sub f2))
    | _ -> assert false
