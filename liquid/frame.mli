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

open Types
open Typedtree
open Format
open Asttypes

type substitution = Path.t * Predicate.pexpr

type open_assignment = Top | Bottom

type qualifier_expr =
    Qvar of (Path.t * open_assignment)  (* Qualifier variable *)
  | Qconst of Qualifier.t list          (* Constant qualifier set *)

type refinement = substitution list * qualifier_expr

val empty_refinement: refinement
val false_refinement: refinement

type t =
    Fvar of Path.t
  | Fconstr of Path.t * t list * variance list * refinement
  | Farrow of pattern_desc option * t * t
  | Ftuple of t list * refinement
  | Frecord of Path.t * (t * string * mutable_flag) list * refinement
  | Funknown

and variance = Covariant | Contravariant | Invariant

val pprint: formatter -> t -> unit
val pprint_fenv: formatter -> t Lightenv.t -> unit list
val pprint_sub: formatter -> substitution -> unit
val pprint_refinement: formatter -> refinement -> unit
val translate_variance: (bool * bool * bool) -> variance
val same_shape: bool -> t -> t -> bool
val translate_pframe: Env.t -> (string * (string * Parsetree.predicate_pattern)) list -> Parsetree.litframe -> t
val fresh: Env.t -> type_expr -> t
val fresh_without_vars: Env.t -> type_expr -> t
val fresh_unconstrained: Env.t -> type_expr -> t
val fresh_with_labels: Env.t -> type_expr -> t -> t
val fresh_constructor: Env.t -> constructor_description -> t -> t list
val instantiate: t -> t -> t
val instantiate_qualifiers: (string * Path.t) list -> t -> t
val bind: Env.t -> pattern_desc -> t -> (Path.t * t) list
val env_bind: Env.t -> t Lightenv.t -> pattern_desc -> t -> t Lightenv.t
val apply_substitution: substitution -> t -> t
val label_like: t -> t -> t
val apply_solution: (Path.t -> Qualifier.t list) -> t -> t
val refinement_conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t list
val refinement_predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t
val refinement_vars: t -> Path.t list
val apply_refinement: refinement -> t -> t
val predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t
val conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t list
