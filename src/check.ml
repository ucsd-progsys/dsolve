open Type
open Expr
open Predicate


let check_type quals exp ty =
  let rec typeof env = function
      Num(n, _) ->
	Int([])
    | ExpVar(x, _) ->
	List.assoc x env
    | _ ->
	raise Not_found
  and check_rec env e t =
    typeof env e = t
  in
    check_rec Builtins.types exp ty
