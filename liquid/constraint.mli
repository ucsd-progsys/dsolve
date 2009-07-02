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

module Sol :
  sig
    type 'a t
    type key = Common.ComparablePath.t
    val find : 'a t -> key -> 'a
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

type fc_id
type subref_id = int

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of Frame.t Lightenv.t * guard_t * Frame.t * Frame.t
  | WFFrame of Frame.t Lightenv.t * Frame.t

type refinement_constraint =
  | SubRef of Frame.refinement (* F.t *) Lightenv.t * guard_t * Frame.refinement * Frame.simple_refinement * (subref_id option)
  | WFRef of Frame.t Lightenv.t * Frame.simple_refinement * (subref_id option)

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t
  | Assert of Location.t
  | Cstr of labeled_constraint

val pprint_fenv : Format.formatter -> Frame.t Lightenv.t -> unit

val fresh_fc_id : unit -> fc_id 

val solve: 
  Parsetree.qualifier_declaration list -> Env.t ->
    int list -> labeled_constraint list ->
    ((Common.ComparablePath.t -> Qualifier.t list) * (labeled_constraint list))

val formals_addn: Frame.qvar list -> unit

val dsolver:
  Frame.t Lightenv.t ->
    (Frame.t * labeled_constraint * refinement_constraint) list ->
      Qualifier.t list Sol.t -> Qualifier.t list Sol.t

(* for fixpoint *)
val solve_with_solver:
  Parsetree.qualifier_declaration list -> Env.t ->
    int list -> labeled_constraint list ->
  (* solver *)
  (Frame.t Lightenv.t ->
    (Frame.t * labeled_constraint * refinement_constraint) list ->
      Qualifier.t list Sol.t -> Qualifier.t list Sol.t) ->
  ((Common.ComparablePath.t -> Qualifier.t list) * (labeled_constraint list))

val sol_of_solmap: Qualifier.t list Lightenv.t -> Qualifier.t list Sol.t
val guard_predicate: unit -> guard_t -> Predicate.t
val env_to_empty_refenv: Frame.t Lightenv.t -> Frame.refinement Lightenv.t
val env_to_refenv: Frame.t Lightenv.t -> Frame.refinement Lightenv.t
val environment_preds: (Path.t -> Qualifier.t list) -> Frame.refinement Lightenv.t -> Predicate.t list
val refinement_preds: (Path.t -> Qualifier.t list) -> Predicate.pexpr -> Frame.refinement -> Predicate.t list
