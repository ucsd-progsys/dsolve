(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: clflags.mli,v 1.1 2005/10/26 13:23:27 doligez Exp $ *)

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val fast : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val output_c_object : bool ref
val ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val preprocessor : string option ref
val save_types : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val use_prims : string ref
val use_runtime : string ref
val principal : bool ref
val recursive_types : bool ref
val make_runtime : bool ref
val gprofile : bool ref
val c_compiler : string ref
val c_linker : string ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val dump_parsetree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_instr : bool ref
val minsol : bool ref
val dump_constraints: bool ref
val dump_ref_constraints: bool ref
val dump_ref_vars: bool ref
val print_subs: bool ref
val dump_queries: bool ref
val dump_frames: bool ref
val raw_frames: bool ref
val dump_graph: bool ref
val no_simple: bool ref
val verify_simple: bool ref
val psimple: bool ref
val simpguard : bool ref
val dont_use_unint_cstrs : bool ref

val hide_rectypes: bool ref
val no_recrefs: bool ref
val no_recvarrefs: bool ref
val ck_mlq: bool ref
val summarize: string option ref
val dsmeasures: bool ref
val use_fixpoint: bool ref
val check_dupenv: bool ref
val dont_mine_mlq_preds: bool ref
val dont_gen_mlq_preds: bool ref
val less_qualifs: bool ref
val no_anormal: bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val inline_threshold : int ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
