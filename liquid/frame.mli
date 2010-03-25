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
type dep_sub      = string * string

type qvar       = Path.t
type refexpr    = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type 'a prerecref = 'a list list
type recref       = refinement prerecref

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

val empty_refinement: refinement
val const_refinement: Qualifier.t list -> refinement
val false_refinement: refinement

type 'a preframe =
  | Fvar       of Ident.t * int * dep_sub list * 'a
  | Fsum       of Path.t * 'a preconstr list * 'a
  | Finductive of Path.t * 'a preparam list * 'a prerecref * 'a preconstr list * 'a
  | Frec       of Path.t * 'a preframe list * 'a prerecref * 'a
  | Fabstract  of Path.t * 'a preparam list * Ident.t * 'a
  | Farrow     of pattern_desc * 'a preframe * 'a preframe

and 'a preparam  = Ident.t * 'a preframe * variance
and 'a preconstr = constructor_tag * (string * 'a preparam list)
and variance     = Covariant | Contravariant | Invariant

type param  = refinement preparam
type constr = refinement preconstr

type t = refinement preframe

exception LabelLikeFailure of t * t

val mutable_variance: Asttypes.mutable_flag -> variance

val generic_level: int

val path_tuple: Path.t

val find_by_name: t Lightenv.t -> string -> t
val prune_env_funs: t Lightenv.t -> Path.t list
val prune_background: 'a Lightenv.t -> 'a Lightenv.t

val sum_of_params: Path.t -> param list -> refinement -> t
val tuple_of_frames: t list -> refinement -> t
val abstract_of_params_with_labels: 
  Ident.t list -> Path.t -> t list -> variance list -> Ident.t -> refinement -> t

val begin_def: unit -> unit
val end_def: unit -> unit
val generalize: t -> t
val initialize_type_expr_levels: type_expr -> unit

val get_recref: t -> recref option
val pprint: formatter -> t -> unit
val pprint_fenv: formatter -> t Lightenv.t -> unit list
val pprint_sub: formatter -> substitution -> unit
val pprint_refinement: formatter -> refinement -> unit
val recref_is_empty: recref -> bool
val mk_refinement: substitution list -> Qualifier.t list -> qvar list -> refinement
val translate_variance: (bool * bool * bool) -> variance
val constr_params: constr -> param list
val constrs_tag_params: constructor_tag -> constr list -> param list
val record_field: t -> int -> t
val iter_labels: (pattern_desc -> unit) -> t -> unit
val map_refexprs: (refexpr -> refexpr) -> t -> t
val map_qualifiers: (Qualifier.t -> Qualifier.t) -> t -> t
val params_frames: param list -> t list
val shape: t -> t
val is_shape: t -> bool
val params_ids: param list -> Ident.t list
val same_shape: t -> t -> bool
val subt: t -> t -> (Ident.t * Ident.t) list -> (Ident.t * t) list -> bool * (Ident.t * Ident.t) list * (Ident.t * t) list
val subti: t -> t -> bool * (Ident.t * Ident.t) list * (Ident.t * t) list
val subtis: t -> t -> bool
val map_inst: (Ident.t * Ident.t) list -> (Ident.t * t) list -> t -> t
(*val translate_pframe: string option -> Env.t -> (string * (string * Parsetree.predicate_pattern)) list -> Parsetree.litframe -> t*)

val unfold: t -> t

val apply: t -> Predicate.pexpr list -> t

val fresh_binder: unit -> pattern_desc
val fresh: Env.t -> type_expr -> t
val fresh_without_vars: Env.t -> type_expr -> t
val fresh_false: Env.t -> type_expr -> t
val fresh_with_labels: Env.t -> type_expr -> t -> t
val fresh_constructed_params_no_vars: Env.t -> Path.t -> t list -> t
val fresh_uninterpreted: Env.t -> type_expr -> Path.t -> t
val uninterpreted_constructors: Env.t -> type_expr -> (string * t) list
val instantiate: t Lightenv.t -> t -> t -> t
val instantiate_qualifiers: (string * Path.t) list -> t -> t
val bind: pattern_desc -> t -> (Path.t * t) list
val env_bind: t Lightenv.t -> pattern_desc -> t -> t Lightenv.t
val refexpr_apply_subs: substitution list -> refexpr -> refexpr
val apply_subs: substitution list -> t -> t
val label_like: t -> t -> t
val apply_solution: (Path.t -> Qualifier.t list) -> t -> t
val refinement_conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t list
val refinement_predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t
val refinement_qvars: refinement -> qvar list
val apply_refinement: refinement -> t -> t
val append_refinement: refinement -> t -> t
val apply_recref_constrs: recref -> constr list -> constr list
val apply_recref: recref -> t -> t
val get_refinement: t -> refinement option 
val has_kvars: t -> bool
val find_tag: refinement -> constructor_tag  option
val refinement_qvars: refinement -> qvar list
val ref_to_simples: refinement -> (simple_refinement list * simple_refinement list)
val ref_of_simple: simple_refinement -> refinement
val refinement_fold : (refinement -> 'a -> 'a) -> 'a -> t -> 'a
val refinement_iter : (refinement -> unit) -> t -> unit
val predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t
val conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t list
