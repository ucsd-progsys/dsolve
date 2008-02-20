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

(* $Id: path.mli,v 1.8 2003/07/01 13:05:43 xleroy Exp $ *)

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

val mk_ident: string -> t

val same: t -> t -> bool
val isfree: Ident.t -> t -> bool
val binding_time: t -> int

val nopos: int

val ident_name: t -> string option
val ident_name_crash: t -> string
val name: t -> string
val unique_name: t -> string
val head: t -> Ident.t
