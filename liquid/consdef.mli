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

module Sol : Hashtbl.S with type key = Frame.qvar

type fc_id = int option

type subref_id = int

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of Frame.t Liqenv.t * guard_t * Frame.t * Frame.t
  | WFFrame of Frame.t Liqenv.t * Frame.t

type refinement_constraint =
  | FixRef of FixConstraint.t
  | SubRef of Frame.refinement Liqenv.t * guard_t * Frame.refinement * Frame.simple_refinement * (subref_id option)
  | WFRef  of Frame.t Liqenv.t * Frame.simple_refinement * (subref_id option)

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

val fresh_fc_id : unit -> fc_id 
val sol_of_solmap: Qualifier.t list Misc.IntMap.t -> Qualifier.t list Sol.t
val solution_map: 'a Sol.t -> Frame.qvar -> 'a 
val guard_predicate: guard_t -> Predicate.t
val environment_preds: (Frame.qvar -> Qualifier.t list) -> Frame.refinement Liqenv.t -> Predicate.t list
val refinement_preds: (Frame.qvar -> Qualifier.t list) -> Predicate.pexpr -> Frame.refinement -> Predicate.t list
val sref_map: (Frame.simple_refinement -> 'a) -> Frame.refinement -> 'a list

val qual_test_var: Path.t
val qual_test_expr: Predicate.pexpr
val is_simple_constraint: refinement_constraint -> bool
val is_simple_constraint2: refinement_constraint -> bool

val is_subref_constraint: refinement_constraint -> bool
val is_wfref_constraint: refinement_constraint -> bool
val is_subframe_constraint: frame_constraint -> bool
val is_wfframe_constraint: frame_constraint -> bool
