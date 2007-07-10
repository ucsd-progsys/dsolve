open Printf
open Predicate
open TheoremProver


(* Type language *)
type qualifier = string * parameterized_pred


type typ =
    Arrow of string * typ * typ
  | Int of qualifier list
  | TyVar of string
  | GenVar of string


let fresh_tyvar = Misc.make_get_fresh (fun x -> TyVar x)


let pprint_quals quals =
  let qual_name (name, _) = name in
    Misc.join (List.map qual_name quals) " "


let rec pprint_type = function
    Arrow(x, (Arrow _ as t1), t2) ->
      sprintf "%s: (%s) -> %s" x (pprint_type t1) (pprint_type t2)
  | Arrow(x, t1, t2) ->
      sprintf "%s: %s -> %s" x (pprint_type t1) (pprint_type t2)
  | Int quals ->
      sprintf "%s int" (pprint_quals quals)
  | TyVar a ->
      sprintf "%s" a
  | GenVar a ->
      sprintf "'%s" a


let rec typ_subst_tyvar b a = function
    Arrow(x, t1, t2) ->
      Arrow(x, typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)
  | TyVar a' when a' = a ->
      b
  | t ->
      t


let generalize_type ty env =
  let (env_dom, _) = List.split env in
  let rec generalize_rec = function
      Arrow(x, t1, t2) ->
        Arrow(x, generalize_rec t1, generalize_rec t2)
    | TyVar a when not (List.mem a env_dom) ->
        GenVar a
    | t ->
        t
  in
    generalize_rec ty


let instantiate_type ty =
  let rec instantiate_rec vars = function
      Arrow(x, t1, t2) ->
        let (t1', vars'') = instantiate_rec vars t1 in
        let (t2', vars') = instantiate_rec vars'' t2 in
          (Arrow(x, t1', t2'), vars')
    | GenVar a ->
        let (t', vars') =
          try
            (List.assoc a vars, vars)
          with Not_found ->
            let t = fresh_tyvar () in
              (t, (a, t)::vars)
        in
          (t', vars')
    | t ->
        (t, vars)
  in
  let (t, _) = instantiate_rec [] ty in
    t


let qualify x (q, PredOver(y, p)) =
  predicate_subst (Var x) y p


let qualifier_subst e x ((q, PredOver(y, p)) as qual) =
  if x = y then
    qual
  else
    (q, PredOver(y, predicate_subst e x p))
