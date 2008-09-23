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

(* $Id: parse.mli,v 1.6 1999/11/17 18:58:19 xleroy Exp $ *)

(* Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure
val interface : Lexing.lexbuf -> Parsetree.signature
val liquid_interface : Lexing.lexbuf -> Parsetree.liquid_sig
val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
val qualifiers : Lexing.lexbuf -> string list * Parsetree.qualifier_declaration list
val qualifier_patterns : Lexing.lexbuf -> Parsetree.qualifier_declaration list
