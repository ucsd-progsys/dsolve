open Predicate

type t = Path.t * Ident.t * predicate

let compare = compare

let apply x (_, y, p) = predicate_subst (Var x) y p

let is_well_formed domain (_, x, p) =
  List.for_all (fun v -> List.mem v (x::domain)) (predicate_vars p)

(* pmr: write a nice, literate expalantion of why this is a good thing to
   have *)
let instantiate names_to_idents (path, valu, pred) =
  (* Don't instantiate the bound variable *)
  let names_to_idents' = (Ident.name valu, valu)::names_to_idents in
    (path, valu, instantiate_named_vars names_to_idents' pred)

