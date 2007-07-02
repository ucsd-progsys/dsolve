open Predicate
open TheoremProver
open Type
open Graph


type subst = (string * expression) list


type frame =
    FArrow of string * frame * frame
  | FVar of subst * string
  | FInt of subst * qualifier list


let fresh_framevar = Misc.make_get_fresh (fun x -> FVar([], x))


let rec frame_to_type = function
    FArrow(x, f, f') ->
      Arrow(x, frame_to_type f, frame_to_type f')
  | FVar(_, x) ->
      TyVar x
  | FInt(_, quals) ->
      Int quals


let frame_apply_subst pexp x fr =
  let s = (x, pexp) in
  let rec apply_subst_rec = function
      FArrow(y, f, f') ->
	FArrow(y, apply_subst_rec f, apply_subst_rec f')
    | FVar(ss, y) ->
	FVar(s::ss, y)
    | FInt(ss, quals) ->
	FInt(s::ss, quals)
  in
    apply_subst_rec fr


let rec pprint_frame = function
    FArrow(x, f, f') ->
      Printf.sprintf "%s: %s -> %s" x (pprint_frame f) (pprint_frame f')
  | FInt(subs, quals) ->
      Printf.sprintf "[%s] %s" (pprint_subst subs) (pprint_quals quals)
  | FVar(subs, x) ->
      Printf.sprintf "[%s] %s" (pprint_subst subs) x
and pprint_subst ss =
  let pprint_single_subst (x, pexp) =
    Printf.sprintf "%s -> %s" x (pprint_expression pexp)
  in
    Misc.join (List.map pprint_single_subst ss) "; "


type subtypconst = SubType of (string * frame) list * predicate * frame * frame


let split_constraints constrs =
  let rec flatten_rec cs flat =
    match cs with
	SubType(env, guard, FArrow(x, f1, f1'), FArrow(y, f2, f2'))::cs ->
	  let param = SubType(env, guard, f2, f1) in
	  let env' = (x, f2)::env in
	  let ret = SubType(env', guard, f1', f2') in
	    flatten_rec (param::ret::cs) flat
      | f::cs ->
	  flatten_rec cs (f::flat)
      | [] ->
	  flat
  in
    flatten_rec constrs []


module Qualifier = struct
  type t = qualifier
  let compare = compare
end


module QualifierSet = Set.Make(Qualifier)


module Solution = struct
  let create base =
    fun _ -> base


  let add k v solution =
    fun k' -> if k = k' then v else solution k'
end


let rec frame_predicate solution (x, f) =
  let (subs, quals) = match f with
      FVar(subst, k) ->
	(subst, QualifierSet.elements (solution k))
    | FInt(subst, qs) ->
	(subst, qs)
    | FArrow(_) ->
	([], [])
  in
  let unsubst = big_and (List.map (qualify x) quals) in
  let substitute (x, e) p = predicate_subst e x p in
    List.fold_right substitute subs unsubst


let environment_predicate solution env =
  big_and (List.map (frame_predicate solution) env)


let constraint_sat solution (SubType(env, guard, f1, f2)) =
      let envp = environment_predicate solution env in
      let p1 = frame_predicate solution ("A", f1) in
      let p2 = frame_predicate solution ("A", f2) in
	Prover.implies (big_and [envp; guard; p1]) p2
