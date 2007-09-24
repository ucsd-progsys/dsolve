open Predicate
open Types
open Btype

type substitution = Ident.t * pexpr

type qualifier_expr =
    Qvar of Ident.t
  | Qconst of Qualifier.t list

type refinement = substitution list * qualifier_expr

type frame_desc =
    Fvar
  | Fconstr of Path.t * frame_expr list * refinement
  | Farrow of Ident.t * frame_expr * frame_expr

and frame_expr = frame_desc ref

let fresh_refinementvar () = ([], Qvar (Ident.create "k"))

(* Create a fresh frame with the same shape as the given type.  Identical tyvars
   becomes references to the same FVar.  (This makes instantiation much
   easier.) *)
let fresh ty =
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
          ref (Fconstr(p, List.map fresh_rec tyl, fresh_refinementvar()))
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
