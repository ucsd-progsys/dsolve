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

type binop =
    Plus
  | Minus
  | Times
  | Div

type binrel =
    Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le 

type pexpr =   
    PInt of int 
  | Var of Path.t
  | FunApp of Path.t * pexpr list 
  | Binop of pexpr * binop * pexpr 
  | Field of Path.t * pexpr     (* INVARIANT: disjoint fields in same module *)
  | Ite of t * pexpr * pexpr

and t =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of t * t
  | Not of t
  | And of t * t 
  | Or of t * t 
  | Forall of (Path.t * Parsetree.prover_t) list * t
  | Exists of (Path.t * Parsetree.prover_t) list * t
  | Implies of t * t
  | Boolexp of pexpr

val pprint_rel: binrel -> string
val pprint: formatter -> t -> unit
val pprint_pexpr: formatter -> pexpr -> unit

val pexp_map_vars: (Path.t -> pexpr) -> pexpr -> pexpr 
val pexp_map_funs: (Path.t -> Path.t) -> pexpr -> pexpr

val map_vars: (Path.t -> pexpr) -> t -> t
val map_funs: (Path.t -> Path.t) -> t -> t

val tag_function: Path.t
val tag: pexpr -> pexpr

val big_and: t list -> t
val big_or: t list -> t
val equals: (pexpr * pexpr) -> t
val implies: (t * t) -> t

val (==.): pexpr -> pexpr -> t
val (!=.): pexpr -> pexpr -> t
val (<=.): pexpr -> pexpr -> t
val (<.): pexpr -> pexpr -> t
val (>=.): pexpr -> pexpr -> t
val (>.): pexpr -> pexpr -> t
val (&&.): t -> t -> t
val (||.): t -> t -> t
val (!.): t -> t
val (=>.): t -> t -> t
val (<=>.): t -> t -> t
val (+-): pexpr -> pexpr -> pexpr
val ( *-): pexpr -> pexpr -> pexpr
val ( /-): pexpr -> pexpr -> pexpr
val (--): pexpr -> pexpr -> pexpr
val (?.): pexpr -> t

val subst: pexpr -> Path.t -> t -> t
val apply_substs: (Path.t * pexpr) list -> t -> t
val vars: t -> Path.t list
val exp_vars: pexpr -> Path.t list
val funs: t -> Path.t list
(* pmr: change to plain old instantiate *)
val instantiate_named_vars: (string * Path.t) list -> t -> t
val transl_op: Asttypes.predexp_op -> binop                                                             
val transl_rel: Asttypes.pred_rel -> binrel

val conjuncts: t -> t list
val is_taut: t -> bool
