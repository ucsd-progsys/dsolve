open Predicate

type t = Path.t * Ident.t * predicate

let compare = compare

let apply x (_, y, p) = predicate_subst (Var x) y p

let is_well_formed domain (_, x, p) =
  List.for_all (fun v -> List.mem v (x::domain)) (predicate_vars p)
