(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: path.ml,v 1.9 2003/07/01 13:05:43 xleroy Exp $ *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

let nopos = -1

let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 && same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
       same fun1 fun2 && same arg1 arg2
  | (_, _) -> false

let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
  | Papply(p1, p2) -> isfree id p1 || isfree id p2

let rec binding_time = function
    Pident id -> Ident.binding_time id
  | Pdot(p, s, pos) -> binding_time p
  | Papply(p1, p2) -> max (binding_time p1) (binding_time p2)

(* temporary hack *)
let ident_name = function
    Pident id -> Some (Ident.name id)
  | _ -> None
let ident_name_crash = function
    Pident id -> Ident.name id 
  | _ -> assert false
let unique_ident_name_crash = function
    Pident id -> Ident.unique_name id
  | _ -> assert false
let unique_ident_name = function
    Pident id -> Some (Ident.unique_name id)
  | _ -> None
let ident_name_fail = function
    Pident id -> Ident.name id
  | _ -> raise (Failure "path")
let is_ident = function
    Pident id -> true
  | _ -> false

let rec name = function
    Pident id -> Ident.name id
  | Pdot(p, s, pos) -> name p ^ "." ^ s
  | Papply(p1, p2) -> name p1 ^ "(" ^ name p2 ^ ")"

let rec unique_name = function
    Pident id -> Ident.unique_name id
  | Pdot(p, s, pos) -> unique_name p ^ "." ^ s
  | Papply(p1, p2) -> unique_name p1 ^ "(" ^ unique_name p2 ^ ")"

let rec head = function
    Pident id -> id
  | Pdot(p, s, pos) -> head p
  | Papply(p1, p2) -> assert false

let mk_ident x = Pident (Ident.create x)

let mk_persistent x = Pident (Ident.create_persistent x)
