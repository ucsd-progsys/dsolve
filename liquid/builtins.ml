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

open Predicate

module C = Common
module F = Frame
module T = Types

let mk_abstract path qs =
  F.Fabstract (path, [], C.abstr_elem_id (), F.const_refinement qs)

let mk_int qs = mk_abstract Predef.path_int qs
let uInt      = mk_int []

let uFloat = mk_abstract Predef.path_float []

let uChar = mk_abstract Predef.path_char []

let mk_string qs = mk_abstract Predef.path_string qs
let uString      = mk_string []

let mk_bool qs = F.Finductive (Predef.path_bool, [],
                               [[]; []],
                               [(T.Cstr_constant 0, ("true", [])); (T.Cstr_constant 1, ("false", []))],
                               F.const_refinement qs)
let uBool = mk_bool []

let array_contents_id = Ident.create "contents"
let mk_array f qs     = F.Fabstract (Predef.path_array, [(array_contents_id, f, F.Invariant)], C.abstr_elem_id (), F.const_refinement qs)

let mk_unit qs     = F.Finductive (Predef.path_unit, [], [[]], [(T.Cstr_constant 0, ("()", []))], F.const_refinement qs)
let uUnit          = mk_unit []
let rUnit name v p = mk_unit [(Path.mk_ident name, v, p)]

let equality_qualifier exp =
  let x = Path.mk_ident "V" in
    let pred = Var x ==. exp in
    Predicate.pprint Format.str_formatter pred;
    let expstr = Format.flush_str_formatter () in (Path.mk_ident expstr, x, pred)

let equality_refinement exp =
  F.const_refinement [equality_qualifier exp]

let int_true = 2
let int_false = 0

let tag_refinement p t =
  let x = Path.mk_ident "V" in
  let ti = C.int_of_tag t in
  let pred = if p = Predef.path_bool then
               let p = Boolexp (Var x) in 
                 if ti = int_true then p else Not p
                   else tag (Var x) ==. PInt (ti) in
  Predicate.pprint Format.str_formatter pred;
  let expstr = Format.flush_str_formatter () in
    F.const_refinement [(Path.mk_ident expstr, x, pred)]

let size_lit_refinement i env =
  let x = Path.mk_ident "x" in
    F.const_refinement
      [(Path.mk_ident "<size_lit_eq>",
        x,
        FunApp(C.lookup_path "Array.length" env, [Var x]) ==. PInt i)]

let field_eq_qualifier name pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<field_eq>", x, Field (Path.Pident name, Var x) ==. pexp)

let proj_eq_qualifier n pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<tuple_nth_eq>", x, Field (Path.Pident (Common.tuple_elem_id n), Var x) ==. pexp)
