open Format

type t = Path.t * Path.t * Predicate.t

let compare = compare

let pprint ppf (_, _, pred) = Predicate.pprint ppf pred

let apply x (_, y, p) = Predicate.subst (Predicate.Var x) y p

exception Refinement_not_closed

(* The user specifies qualifiers as open predicates - i.e., the variables named
   in the qualifier may not yet be in scope at the time of definition.  But
   we want qualifiers that refer to OCaml paths, which are unique, not
   variable names, which can appear multiple times.  Using variable names
   instead of unique paths would cause trouble with the following expression:

   let a = 1 in
   let x = a + 1 in   (* x has type {v : int | v > a} *)
   let a = 3 in ...   (* x's type is now incorrect *)

   This function replaces all instances of named variables in a qualifier with
   the unique paths of the same name in the given environment.  It raises
   Refinement_not_closed if a variable in the qualifier is not found in the
   environment. *)
let instantiate env (path, valu, pred) =
  (* Don't instantiate the bound variable *)
  let names_to_paths =
     (Path.name valu, valu) ::
       (Lightenv.maplistfilter (fun path _ -> let name = Path.ident_name path in match name with Some name -> Some (name, path) | None -> None) env) in
    try Some (path, valu, Bstats.time "instantiating" (Predicate.instantiate_named_vars names_to_paths) pred)
    with Not_found -> None
