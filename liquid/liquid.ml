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

open Config
open Format
open Liqerrors
open Misc
open Types
open Clflags
open Gc

module F = Frame
module MLQ = Mlqmod
module M = Measure
module C = Common
module Le = Lightenv
module P = Predicate

let usage = "Usage: liquid <options> [source-files]\noptions are:"

let filenames = ref []

let file_argument fname =
  filenames := !filenames @ [fname]

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    failwith "cannot open pervasives.cmi"

let initial_fenv env = Lightenv.addn (Builtins.frames env) Lightenv.empty

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let type_implementation initial_env ast =
  Typecore.reset_delayed_checks ();
  let str = Typemod.type_structure initial_env ast in
    Typecore.force_delayed_checks ();
    str

let analyze ppf sourcefile (str, env, fenv, ifenv) =
  Qualifymod.qualify_implementation sourcefile fenv ifenv env [] str

let load_qualfile ppf qualfile =
  let (deps, qs) = Pparse.file ppf qualfile Parse.qualifiers ast_impl_magic_number in
    (deps, List.map Qualmod.type_qualifier qs)

let load_dep_mlqfiles bname deps env fenv mlqenv =
  let pathnames = !Config.load_path in 
  let inames = List.map (fun s -> ((String.lowercase s) ^ ".mlq", s)) deps in
  let inames = C.sort_and_compact inames in
  let f (s, d) pns =
    let p = try Some (List.find (fun p -> Sys.file_exists (p ^ "/" ^ s)) pns) with Not_found -> None in
      match p with
      | Some p -> Some (Pparse.file std_formatter (p ^ "/" ^ s) Parse.liquid_interface Config.ast_impl_magic_number, d)
      | None -> None in
  let mlqs = List.rev_map (fun xd -> f xd pathnames) inames in 
  let mlqs = C.maybe_list mlqs in
    MLQ.load_dep_sigs env fenv mlqs

let dump_qualifiers bname (str, env, menv, ifenv) qname = 
  let deps = Qualgen.all_modules str in
  let (env, emenv, efenv, _) = load_dep_mlqfiles bname deps env Le.empty Le.empty in
  let (menv, ifenv) = (List.rev_append menv emenv, Le.combine efenv ifenv) in
  let ifenv = MLQ.scrub_axioms ifenv in
    Qdump.dump_default_qualifiers (str, env, menv, ifenv) deps qname

let load_valfile ppf env fenv fname =
  try
    let (preds, decls) = MLQ.parse ppf fname in
    let vals = MLQ.filter_vals decls in
    let kvl = List.map (fun (s, pf) -> (s, F.translate_pframe None env preds pf)) vals in
    let tag = (Path.mk_ident P.tag_function, F.Fvar(Path.mk_ident "", 0, [], F.empty_refinement)) in
    let f = (fun (k, v) -> (C.lookup_path k env, F.label_like v v)) in
    let kvl = tag :: (List.map f kvl) in
      (env, Lightenv.addn kvl fenv)
  with Not_found -> failwith (Printf.sprintf "builtins: val %s does not correspond to library value" fname)
    
let load_sourcefile ppf env fenv sourcefile =
  let str = Pparse.file ppf sourcefile Parse.implementation ast_impl_magic_number in 
  let str = if !Clflags.no_anormal then str else Normalize.normalize_structure str in
  let str = print_if ppf Clflags.dump_parsetree Printast.implementation str in
  let (str, _, env) = type_implementation env str in
    (str, env, fenv)

let dump_env env = printf "(Pruned background env)@.%a@." Constraint.pprint_fenv env

let add_uninterpreted_constructors tenv fenv (id, td) =
  match Env.constructors_of_type (Path.Pident id) td with
    | (_, {cstr_res = ty}) :: _ ->
        let ucs = F.uninterpreted_constructors tenv ty in
          Le.addn (List.map (fun (n, f) -> (Path.mk_persistent n, f)) ucs) fenv
    | _ -> fenv

let process_sourcefile env fenv fname =
  let bname = Misc.chop_extension_if_any fname in
  let (qname, iname) = (bname ^ ".quals", bname ^ ".mlq") in
  try
    (* We need to pull out uninterpreted functions from the MLQ in order to typecheck. *)
    let (preds, vals)         = MLQ.parse std_formatter iname in
    let (unints, vals)        = List.partition MLQ.is_unint_decl vals in
    let (env, _, fenv, _)     = MLQ.load_local_sig env fenv ([], unints) in
    let (str, env, fenv)      = load_sourcefile std_formatter env fenv fname in
    let fenv                  = List.fold_left (add_uninterpreted_constructors env) fenv (Env.types env) in
    let (env, menv, fenv, mlqenv) = MLQ.load_local_sig env fenv (preds, vals) in
      if !dump_qualifs then
        dump_qualifiers bname (str, env, menv, mlqenv) qname
      else
        let (deps, quals)              = load_qualfile std_formatter qname in
        let (env, menv', fenv, _)      = load_dep_mlqfiles bname deps env fenv mlqenv in
        let (fenv, mlqenv, quals) = M.proc_premeas env (List.rev_append menv menv') fenv mlqenv quals in
        let fenv = MLQ.scrub_and_push_axioms fenv in
        let _ = if C.ck_olev C.ol_dump_quals then List.iter (function | Typedtree.Tstr_qualifier(a, (b, c)) -> printf "@[%a@]@." Qualifier.pprint (Path.Pident a, Path.Pident b, c) | _ -> assert false) quals in
        let _ = if C.ck_olev C.ol_dump_env then (dump_env fenv; dump_env mlqenv) else () in
          analyze std_formatter fname (List.rev_append quals str, env, fenv, mlqenv)
   with x ->
     report_error std_formatter x; exit 1

let process_file (env, fenv) fname =
  match Misc.get_extension fname with
    | Some "ml" ->
        (* odd things may happen if multiple files are run through here *)
        process_sourcefile env fenv fname;
        (env, fenv)
    | Some "mlq" ->
        load_valfile std_formatter env fenv fname
    | _ ->
        failwith (sprintf "Unrecognized file type for file %s" fname)

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir ->
       let dir = expand_directory Config.standard_library dir in
       include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-init", Arg.String (fun s -> init_file := Some s),
           "<file>  Load <file> instead of default init file";
     "-labels", Arg.Clear classic, " Labels commute (default)";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-nolabels", Arg.Set classic, " Ignore labels and do not commute";
     "-noprompt", Arg.Set noprompt, " Suppress all prompts";
     "-nostdlib", Arg.Set no_std_include,
           " do not add default directory to the list of include directories";
     "-principal", Arg.Set principal, " Check principality of type inference";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    E/e enable/disable fragile match\n\
       \032    F/f enable/disable partially applied function\n\
       \032    L/l enable/disable labels omitted in application\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    Y/y enable/disable suspicious unused variables\n\
       \032    Z/z enable/disable all other unused variables\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Aelz\"";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Treat the warnings of <flags> as errors, if they are enabled.\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)";
     "-minsol", Arg.Set minsol, "compute minimum solutions";
     "-dconstrs", Arg.Set dump_constraints, "print out frame constraints";
     "-drconstrs", Arg.Set dump_ref_constraints, "print out refinement constraints";
     "-dsubs", Arg.Set print_subs, "print subs and unsubbed predicates";
     "-drvars", Arg.Set dump_ref_vars, "print out variables associated with refinement constraints";
     "-dqexprs", Arg.Set dump_qexprs, "print out all subexpressions with their qualified types";
     "-dqualifs", Arg.String (fun s -> dump_qualifs := true; Qdump.patf := s), "<file> dump qualifiers for patterns in <file>";
     "-dqueries", Arg.Set dump_queries, "print out all theorem prover queries and their results";
     "-dframes", Arg.Set dump_frames, "place frames in an annotation file";
     "-draw", Arg.Set raw_frames, "use raw frame templates in annotation file";
     "-dgraph", Arg.Set dump_graph, "dump constraints.dot";
     "-lqueries", Arg.Set log_queries, "log queries to [prover].log";
     "-cqueries", Arg.Set check_queries, "use a backup prover to check all queries";
     "-bquals", Arg.Set brief_quals, "print out the number of refinements for a type instead of their names";
     "-esimple", Arg.Set esimple, "simplify e-variables for rectypes";
     "-no-simple", Arg.Set no_simple, "do not propagate in simple constraints";
     "-no-simple-subs", Arg.Set no_simple_subs, "do not propagate sets when substitutions are present";
     "-verify-simple", Arg.Set verify_simple, "verify simple constraint propagation against theorem prover result";
     "-use-list", Arg.Set use_list, "use worklist instead of heap in solver";
     "-bprover", Arg.Set always_use_backup_prover, "always use backup prover";
     "-qprover", Arg.Set use_qprover , "use Qprover";
     "-qpdump", Arg.Set qpdump, "dump Qprover queries";
     "-lqualifs", Arg.Set less_qualifs, "only collect formal parameter identifiers";
     "-no-anormal", Arg.Set no_anormal, "don't rewrite the AST for a-normality";
     "-ksimpl", Arg.Set kill_simplify, "kill simplify after a large number of queries to reduce memory usage";
     "-cacheq", Arg.Set cache_queries, "cache theorem prover queries";
     "-psimple", Arg.Set psimple, "prioritize simple constraints";
     "-simpguard", Arg.Set simpguard, "simplify guard (remove iff)";
     "-no-recrefs", Arg.Set no_recrefs, "true out recursive refinements";
     "-no-recvarrefs", Arg.Set no_recvarrefs, "true out top-level recvar refinements";
     "-check-mlq", Arg.Set ck_mlq, "warn about possible errors in the local mlq";
     "-union-wfs", Arg.Set union_wfs, "take the union of instantiated wf quals";
     "-no-timing", Arg.Unit Bstats.dont_time, "don't do any profiling";
     "-vgc", Arg.Int (fun c -> (get ()).verbose <- c), "verbose garbage collector";
     "-v", Arg.Int (fun c -> Common.verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output";
     "-collect", Arg.Int (fun c -> Qualgen.col_lev := c), "[1] number of lambdas to collect identifiers under";
  ] file_argument usage;
  init_path ();
  let env = initial_env () in
    ignore (List.fold_left process_file (env, initial_fenv env) !filenames)

let _ = 
  main (); exit 0
