open Predicate
open Type


type subst = (string * pexpr) list

type frame =
    FArrow of string * frame * frame
  | FList of frame
  | FVar of subst * string
  | FTyVar of string
  | FInt of subst * qualifier list
  | GenFrame of string list * frame

let fresh_framevar = Misc.make_get_fresh (fun x -> FVar([], String.uppercase x))


let pprint_subst subs =
  Misc.join (List.map (fun (x, e) -> Printf.sprintf "%s -> %s" x (pprint_pexpr e)) subs) "; "


let rec pprint_frame = function
    FArrow(x, (FArrow _ as f), f') ->
      Printf.sprintf "(%s) -> %s" (pprint_frame f) (pprint_frame f')
  | FArrow(x, f, f') ->
      Printf.sprintf "%s: %s -> %s" x (pprint_frame f) (pprint_frame f')
  | FList f ->
      Printf.sprintf "%s list" (pprint_frame f)
  | FInt(subs, quals) ->
      Printf.sprintf "[%s] %s int" (pprint_subst subs) (pprint_quals quals)
  | FVar(subs, a) ->
      Printf.sprintf "[%s] %s" (pprint_subst subs) a
  | FTyVar a ->
      Printf.sprintf "'%s" a
  | GenFrame(_, f) ->
      pprint_frame f


let frame_apply_subst ((x, pexp) as s) fr =
  let rec apply_subst_rec = function
      FArrow(y, f, f') ->
	FArrow(y, apply_subst_rec f, apply_subst_rec f')
    | FList f ->
        FList(apply_subst_rec f)
    | FVar(ss, y) ->
	FVar(s::ss, y)
    | FInt(ss, quals) ->
	FInt(s::ss, quals)
    | FTyVar a ->
        FTyVar a
    | GenFrame(vars, f) ->
        GenFrame(vars, apply_subst_rec f)
  in apply_subst_rec fr


(* Generalize a frame by following the pattern of a type.  First adds all the qualifiers
   from the type to the frame, then replaces the frame's tyvars with the type's tyvars. *)
let rec generalize_frame_like_typ f t =
  match (f, t) with
      (f, GenTy(vars, t')) ->
        GenFrame(vars, generalize_frame_like_typ f t')
    | (FArrow(x, f, f'), Arrow(_, t, t')) ->
        FArrow(x, generalize_frame_like_typ f t, generalize_frame_like_typ f' t')
    | (FList f, List t) ->
        FList (generalize_frame_like_typ f t)
    | (f, Int) ->
        f
    | (FTyVar _, TyVar a) ->
        FTyVar a
    | _ -> failwith (Printf.sprintf "Cannot generalize frame like type: shapes don't match (%s vs %s)" (pprint_frame f) (pprint_typ t))


let frame_subst_ftyvar b a f =
  let rec subst_rec = function
      FArrow(x, f, f') ->
        FArrow(x, subst_rec f, subst_rec f')
    | FList f ->
        FList (subst_rec f)
    | GenFrame(vars, f) when not (List.mem a vars) ->
        GenFrame(vars, subst_rec f)
    | FTyVar a' when a = a' ->
        b
    | f -> f
  in subst_rec f


(* Instantiate a generalized frame, replacing its generalized variables with fresh ones. *)
let instantiate_frame = function
    GenFrame(vars, f) ->
      List.fold_left (fun f' v -> frame_subst_ftyvar (fresh_framevar()) v f') f vars
  | f -> f


let rec fresh_frame_from_typ = function
    Arrow(x, t, t') ->
      FArrow(x, fresh_frame_from_typ t, fresh_frame_from_typ t')
  | List t ->
      FList(fresh_frame_from_typ t)
  | TyVar a ->
      FTyVar a
  | GenTy(_, t) ->
      (* Ditch quantifiers - if needed, they can be generated later using
         generalize_frame_like_typ *)
      fresh_frame_from_typ t
  | Int -> fresh_framevar()
