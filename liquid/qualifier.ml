(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

open Format

type t = Path.t * Path.t * Predicate.t

let compare = compare

let pprint ppf (_, _, pred) = Predicate.pprint ppf pred

let apply x (_, y, p) = Predicate.subst x y p

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
let instantiate varmap (path, valu, pred) =
  (* Don't instantiate the bound variable *)
  let varmap = (Path.ident_name_crash valu, valu) :: varmap in
    try Some (path, valu, Predicate.instantiate_named_vars varmap pred)
    with Not_found -> None
