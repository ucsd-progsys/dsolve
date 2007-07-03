open Printf
open Predicate
open TheoremProver


(* Type language *)
type qualifier = string * parameterized_pred


type typ =
    Arrow of string * typ * typ
  | Int of qualifier list
  | TyVar of string


let rec typ_subst_tyvar b a = function
  | Arrow(x, t1, t2) ->
      Arrow(x, typ_subst_tyvar b a t1, typ_subst_tyvar b a t2)
  | TyVar a' when a' = a ->
      b
  | t ->
      t


let const_int_quals quals guard n =
  let n_exp = PInt n in
  let n_satisfies (_, PredOver(x, p)) =
    Prover.implies guard (predicate_subst n_exp x p)
  in
    List.filter n_satisfies quals


let qualify x (q, PredOver(y, p)) =
  predicate_subst (Var x) y p


let qualifier_subst e x ((q, PredOver(y, p)) as qual) =
  if x = y then
    qual
  else
    (q, PredOver(y, predicate_subst e x p))


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
      sprintf "'%s" a
