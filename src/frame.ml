open Predicate
open Type


type subst = (string * pexpr) list

type refinementexpr =
    RVar of string
  | RQuals of qualifier list

let fresh_refinementvar = Misc.make_get_fresh (fun x -> RVar x)

type refinement = subst * refinementexpr

type frame =
    FArrow of string * frame * frame
  | FList of frame
  | FTyVar of string
  | FBase of basetyp * refinement


let pprint_subst subs =
  Misc.join (List.map (fun (x, e) -> Printf.sprintf "%s -> %s" x (pprint_pexpr e)) subs) "; "

let pprint_refinementexpr = function
    RVar k -> k
  | RQuals qs -> pprint_quals qs

let pprint_refinement (s, rexpr) = Printf.sprintf "[%s] %s" (pprint_subst s) (pprint_refinementexpr rexpr)

let rec pprint_frame = function
    FArrow(x, (FArrow _ as f), f') ->
      Printf.sprintf "(%s) -> %s" (pprint_frame f) (pprint_frame f')
  | FArrow(x, f, f') ->
      Printf.sprintf "%s: %s -> %s" x (pprint_frame f) (pprint_frame f')
  | FList f ->
      Printf.sprintf "%s list" (pprint_frame f)
  | FBase(b, r) ->
      Printf.sprintf "%s %s" (pprint_refinement r) (pprint_basetyp b)
  | FTyVar a ->
      Printf.sprintf "'%s" a


let frame_apply_subst s fr =
  let rec apply_subst_rec = function
      FArrow(y, f, f') ->
	FArrow(y, apply_subst_rec f, apply_subst_rec f')
    | FList f ->
        FList(apply_subst_rec f)
    | FBase(b, (ss, rexpr)) ->
	FBase(b, (s::ss, rexpr))
    | FTyVar a ->
        FTyVar a
  in apply_subst_rec fr


let frame_subst_ftyvar b a f =
  let rec subst_rec = function
      FArrow(x, f, f') ->
        FArrow(x, subst_rec f, subst_rec f')
    | FList f ->
        FList (subst_rec f)
    | FTyVar a' when a = a' ->
        b
    | f -> f
  in subst_rec f


let rec fresh_frame_from_typ = function
    Arrow(x, t, t') ->
      FArrow(x, fresh_frame_from_typ t, fresh_frame_from_typ t')
  | List t ->
      FList(fresh_frame_from_typ t)
  | TyVar a ->
      FTyVar a
  | GenTy(_, t) ->
      fresh_frame_from_typ t
  | Base b -> FBase(b, ([], fresh_refinementvar()))


(* For each instantiated variable in the frame, replace all instances of that
   variable with a fresh frame based on the type used for instantiation. *)
let instantiate_frame_like_typ fr ty =
  let rec instantiate_rec sub = function
    | (FArrow(_, f1, f2), Arrow(_, t1, t2)) ->
        let sub' = instantiate_rec sub (f1, t1) in
          instantiate_rec sub' (f2, t2)
    | (FList f, List t) ->
        instantiate_rec sub (f, t)
    | (FTyVar a, t) ->
        (fun f -> frame_subst_ftyvar (fresh_frame_from_typ t) a (sub f))
    | (FBase _, _) ->
        sub
    | _ -> failwith "Mismatched frame/type in instantiate_frame_like_typ"
  in
  let sub = instantiate_rec (fun f -> f) (fr, ty) in sub fr
