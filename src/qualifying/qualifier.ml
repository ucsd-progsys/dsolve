open Format

type t = Path.t * Path.t * Predicate.t

let pprint ppf (path, _, _) =
  fprintf ppf "%s" (Path.name path)

let compare = compare

let apply x (_, y, p) = Predicate.subst (Predicate.Var x) y p

exception Refinement_not_closed

(* The user specifiers qualifiers as open predicates - i.e., the variables named
   in the qualifier may not yet be in scope at the time of definition.  But
   we want qualifiers that refer to OCaml identifiers, which are unique, not
   variable names, which can appear multiple times.  Using variable names
   instead of unique ids would cause trouble with the following expression:

   let a = 1 in
   let x = a + 1 in   (* x has type {v : int | v > a} *)
   let a = 3 in ...   (* x's type is now incorrect *)

   This function replaces all instances of named variables in a qualifier with
   the unique identifiers of the same name in the given environment.  It raises
   Refinement_not_closed if a variable in the qualifier is not found in the
   environment. *)
let instantiate env (path, valu, pred) =
  (* Don't instantiate the bound variable *)
  let names_to_idents =
     (Path.name valu, valu) ::
       (Lightenv.maplist (fun path _ -> (Path.name path, path)) env) in
    try
      (path, valu, Predicate.instantiate_named_vars names_to_idents pred)
    with Not_found -> raise Refinement_not_closed
