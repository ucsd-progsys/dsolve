open Printf
open Predicate
open TheoremProver


(* Type language *)
type basetyp =
    Int
  | Bool

type typ =
    Arrow of string * typ * typ
  | List of typ
  | Base of basetyp
  | TyVar of string
  | GenTy of string list * typ

let (fresh_tyvar, reset_fresh_tyvar) = Misc.make_get_fresh_and_reset (fun x -> TyVar x)

let pprint_quals quals = Misc.join (List.map (fun (x, _) -> x) quals) " "

let pprint_basetyp = function
  | Int -> "int"
  | Bool -> "bool"

let rec pprint_typ = function
    Arrow(x, (Arrow _ as t1), t2) ->
      sprintf "%s: (%s) -> %s" x (pprint_typ t1) (pprint_typ t2)
  | Arrow(x, t1, t2) ->
      sprintf "%s: %s -> %s" x (pprint_typ t1) (pprint_typ t2)
  | List t ->
      sprintf "%s list" (pprint_typ t)
  | Base b ->
      pprint_basetyp b
  | TyVar a ->
      sprintf "'%s" a
  | GenTy(_, t) ->
      pprint_typ t


let rec typ_subst_tyvar b a = function
    Arrow(x, t1, t2) ->
      Arrow(x, typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)
  | List t ->
      List(typ_subst_tyvar b a t)
  | TyVar a' when a' = a ->
      b
  | GenTy(vars, t) when not (List.mem a vars) ->
      GenTy(vars, typ_subst_tyvar b a t)
  | t -> t


(* Return the variables free in a given type, assuming the variables in
   bound are already bound (either in the environment or an enclosing GenTy). *)
let typ_free_vars bound =
  let rec free_rec bnd vars = function
      Arrow(_, t1, t2) ->
        let vars' = free_rec bnd vars t1 in
          free_rec bnd vars' t2
    | List t ->
        free_rec bnd vars t
    | TyVar a when not (List.mem a bnd) ->
        a::vars
    | GenTy(genbound, t) ->
        free_rec (genbound@bnd) vars t
    | _ -> vars
  in free_rec bound []


(* Generalize a type over those of its free variables which are not also
   free in the environment. *)
let generalize_typ ty env =
  let env_range = Env.maplist (fun _ t -> t) env in
  let env_free_tyvars = Misc.flap (typ_free_vars []) env_range in
    match typ_free_vars env_free_tyvars ty with
        [] -> ty
      | vars -> GenTy(vars, ty)


(* Instantiate a generalized type, replacing its generalized variables with
   fresh ones. *)
let instantiate_typ = function
    GenTy(vars, t) ->
      List.fold_left (fun t' v -> typ_subst_tyvar (fresh_tyvar()) v t') t vars
  | t -> t


let qualifier_subst e x ((q, PredOver(y, p)) as qual) =
  if x = y then
    qual
  else
    (q, PredOver(y, predicate_subst e x p))
