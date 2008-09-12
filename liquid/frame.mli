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

type qvar = Path.t
type refexpr = substitution list * (Qualifier.t list * qvar list)
type refinement = refexpr list

type recref = refinement list list

type qexpr =
  | Qconst of Qualifier.t
  | Qvar of qvar

type simple_refinement = substitution list * qexpr

val empty_refinement: refinement
val const_refinement: Qualifier.t list -> refinement
val false_refinement: refinement

type t =
  | Fvar of Path.t * int * refinement
  | Frec of Path.t * recref * refinement
  | Fsum of Path.t * (Path.t * recref) option * constr list * refinement
  | Fabstract of Path.t * param list * refinement
  | Farrow of pattern_desc option * t * t
  | Funknown

and param = Ident.t * t * variance

and constr = constructor_tag * param list

and variance = Covariant | Contravariant | Invariant

val generic_level: int

val path_tuple: Path.t

val record_of_params: Path.t -> param list -> refinement -> t
val tuple_of_frames: t list -> refinement -> t

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
val constrs_params: constr list -> param list
val map_refexprs: (refexpr -> refexpr) -> t -> t
val params_frames: param list -> t list
val shape: t -> t
val is_shape: t -> bool
val params_ids: param list -> Ident.t list
val same_shape: t -> t -> bool
val subt: t -> t -> bool
val translate_pframe: string option -> Env.t -> (string * (string * Parsetree.predicate_pattern)) list -> Parsetree.litframe -> t
val replace_recvar: t -> t -> t
val unfold: t -> t
val unfold_applying: t -> t
val fresh: Env.t -> type_expr -> t
val fresh_without_vars: Env.t -> type_expr -> t
val fresh_false: Env.t -> type_expr -> t
val fresh_with_labels: Env.t -> type_expr -> t -> t
val fresh_uninterpreted: Env.t -> type_expr -> string -> t
val instantiate: t -> t -> t
val instantiate_qualifiers: (string * Path.t) list -> t -> t
val bind: pattern_desc -> t -> (Path.t * t) list
val env_bind: t Lightenv.t -> pattern_desc -> t -> t Lightenv.t
val apply_subs: substitution list -> t -> t
val label_like: t -> t -> t
val apply_solution: (Path.t -> Qualifier.t list) -> t -> t
val refinement_conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t list
val refinement_predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> refinement -> Predicate.t
val apply_refinement: refinement -> t -> t
val append_refinement: refinement -> t -> t
val apply_recref_constrs: recref -> constr list -> constr list
val apply_recref: recref -> t -> t
val get_refinement: t -> refinement option 
val int_of_tag: constructor_tag -> int
val tag_of_int: int -> constructor_tag
val tag_function: string
val find_tag: refinement -> constructor_tag  option
val refinement_qvars: refinement -> qvar list
val ref_to_simples: refinement -> (simple_refinement list * simple_refinement list)
val ref_of_simple: simple_refinement -> refinement
val predicate:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t
val conjuncts:
  (Path.t -> Qualifier.t list) -> Predicate.pexpr -> t -> Predicate.t list
