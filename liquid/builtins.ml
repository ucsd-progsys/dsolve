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

open Longident
open Typedtree
open Predicate
open Frame
open Asttypes
open Types

module C = Common

let rec mk_longid = function
  | [] -> assert false
  | [id] -> Lident id
  | id :: idrem -> Ldot (mk_longid idrem, id)

let fake s = Path.mk_ident s
let faketable = Hashtbl.create 17
let fake s = C.do_memo faketable fake s s

let qdim rel dim x y z =
  let dimstr = string_of_int dim in
    (Path.mk_ident ("DIM" ^ dimstr ^ (pprint_rel rel)), y,
     Atom(Var z, rel, FunApp(fake ("Bigarray.Array2.dim" ^ dimstr), [Var x])))

let qint rel i y =
  (Path.mk_ident (Printf.sprintf "INT_%s%d" (pprint_rel rel) i), y, Atom(Var y, rel, PInt i))

let mk_tyvar () = Frame.Fvar(Path.mk_ident "a", Frame.generic_level, [], empty_refinement)

let mk_abstract path qs =
  Fabstract(path, [], C.abstr_elem_id (), const_refinement qs)

let mk_int qs = mk_abstract Predef.path_int qs

let uFloat = mk_abstract Predef.path_float []

let uChar = mk_abstract Predef.path_char []

let mk_string qs = mk_abstract Predef.path_string qs

let uString = mk_string []

let mk_bool qs = Fsum(Predef.path_bool, None, [(Cstr_constant 0, ("true", [])); (Cstr_constant 1, ("false", []))], const_refinement qs)
let uBool = mk_bool []

let array_contents_id = Ident.create "contents"
let mk_array f qs = Fabstract(Predef.path_array, [(array_contents_id, f, Invariant)], C.abstr_elem_id (), const_refinement qs)

let find_constructed_type id env =
  let path =
    try fst (Env.lookup_type (mk_longid id) env)
    with Not_found -> Printf.printf "Couldn't load %s!\n" (String.concat " " id); assert false
  in
  let decl = Env.find_type path env in (path, List.map translate_variance decl.type_variance)

let mk_named id fs qs env =
  let (path, varis) = find_constructed_type id env in
  let fresh_names = Misc.mapi (fun _ i -> Common.tuple_elem_id i) varis in
    Fabstract(path, Common.combine3 fresh_names fs varis, C.abstr_elem_id (), const_refinement qs)

let ref_contents_id = Ident.create "contents"
let mk_ref f env =
  record_of_params (fst (find_constructed_type ["ref"; "Pervasives"] env)) [(ref_contents_id, f, Invariant)] empty_refinement

let mk_bigarray_kind a b qs env = mk_named ["kind"; "Bigarray"] [a; b] qs env

let mk_bigarray_layout a qs env = mk_named ["layout"; "Bigarray"] [a] qs env

let mk_bigarray_type a b c qs env = mk_named ["t"; "Array2"; "Bigarray"] [a; b; c] qs env

let mk_unit qs = Fsum(Predef.path_unit, None, [(Cstr_constant 0, ("()", []))], const_refinement qs)
let uUnit = mk_unit []
let rUnit name v p = mk_unit [(Path.mk_ident name, v, p)]

let uInt = mk_int []

let char = ref 0

let reset_idents () =
  char := Char.code 'a' - 1

let fresh_ident () =
  incr char; String.make 1 (Char.chr (!char))

let def f =
  let (x, y) = (Path.mk_ident (fresh_ident ()), Path.mk_ident (fresh_ident ())) in
  let (f, fy) = f x in
  let xid = match x with
  | Path.Pident id -> id
  | _ -> assert false
  in Farrow (Tpat_var xid, f, fy y)

let defun f =
  reset_idents (); def f

let (==>) x y = (x, y)

let (===>) x y = x ==> fun _ -> def y

let forall f = f (mk_tyvar ())

let bigarray_dim_frame dim env =
  (["dim" ^ string_of_int dim; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun r -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ==>
          fun s -> mk_int [qdim Eq dim r s s; qint Gt 0 s])))

let _lib_frames env = [
  (["create"; "Array2"; "Bigarray"],
   defun (forall (fun a -> forall (fun b -> forall (fun c ->
          fun k -> mk_bigarray_kind a b [] env ===>
          fun l -> mk_bigarray_layout c [] env ===>
          fun dim1 -> mk_int [qint Gt 0 dim1] ===>
          fun dim2 -> mk_int [qint Gt 0 dim2] ==>
          fun z -> mk_bigarray_type a b c [qdim Eq 1 z z dim1; qdim Eq 2 z z dim2] env)))));

  (["get"; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun r -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ===>
          fun i -> mk_int [qint Ge 0 i; qdim Lt 1 r i i] ===>
          fun j -> mk_int [qint Ge 0 j; qdim Lt 2 r j j] ==>
          fun _ -> a)));

  (["set"; "Array2"; "Bigarray"],
   defun (forall (fun a ->
          fun u -> mk_bigarray_type a (mk_tyvar ()) (mk_tyvar ()) [] env ===>
          fun i -> mk_int [qint Ge 0 i; qdim Lt 1 u i i] ===>
          fun j -> mk_int [qint Ge 0 j; qdim Lt 2 u j j] ===>
          fun v -> a ==>
          fun _ -> uUnit)));

  bigarray_dim_frame 1 env;
  bigarray_dim_frame 2 env;
]

let _type_path_constrs env = [
  ("ref", find_constructed_type ["ref"; "Pervasives"] env);
  ("array2", find_constructed_type ["t"; "Array2"; "Bigarray"] env);
]

let _type_paths = ref None

let ext_find_type_path t =
  let (_, (path, _)) = (List.find (fun (a, _) -> (a = t))
                          (match !_type_paths with None -> assert false
                             | Some b -> b))
  in path

let find_path id env = fst (Env.lookup_value (mk_longid id) env)
let find_path_path env p = fst (Env.lookup_value (Longident.parse (Path.name p)) env)

let frames env =
  let _ = _type_paths := Some (_type_path_constrs env) in
  let resolve_names x = List.map (fun (id, fr) -> (find_path id env, fr)) x in
  let frames = resolve_names (_lib_frames env) in
    List.rev_map (fun (p, fr) -> (p, map_qualifiers
      (fun (p1, p2, p) -> (p1, p2, map_funs (find_path_path env) p)) fr)) frames

let equality_qualifier exp =
  let x = Path.mk_ident "V" in
    let pred = Var x ==. exp in
    Predicate.pprint Format.str_formatter pred;
    let expstr = Format.flush_str_formatter () in (Path.mk_ident expstr, x, pred)

let equality_refinement exp =
  const_refinement [equality_qualifier exp]

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
    const_refinement [(Path.mk_ident expstr, x, pred)]

let size_lit_refinement i env =
  let x = Path.mk_ident "x" in
    const_refinement
      [(Path.mk_ident "<size_lit_eq>",
        x,
        FunApp(Common.lookup_path "Array.length" env, [Var x]) ==. PInt i)]

let field_eq_qualifier name pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<field_eq>", x, Field (Path.Pident name, Var x) ==. pexp)

let proj_eq_qualifier n pexp =
  let x = Path.mk_ident "x" in (Path.mk_ident "<tuple_nth_eq>", x, Field (Path.Pident (Common.tuple_elem_id n), Var x) ==. pexp)
