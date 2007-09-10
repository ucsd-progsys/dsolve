(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pathname.mli,v 1.1 2007/02/07 08:59:14 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
include Signatures.PATHNAME

val is_up_to_date : bool -> t -> bool
val clean_up_links : bool Slurp.entry -> bool Slurp.entry
val exists_in_source_dir : t -> bool
val exists_in_build_dir : t -> bool
val import_in_build_dir : t -> unit
val in_build_dir : t -> t
val in_source_dir : t -> t
