open Config
open Format
open Liqerrors
open Misc

let usage = "Usage: liquid <options> [source-file]\noptions are:"

let filename = ref ""

let file_argument fname = filename := fname

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

let (++) x f = f x

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let type_implementation initial_env ast =
  Typecore.reset_delayed_checks ();
  let str = Typemod.type_structure initial_env ast in
    Typecore.force_delayed_checks ();
    str

let analyze ppf sourcefile (str, env, fenv) =
  Qualifymod.qualify_implementation sourcefile fenv [] str

open Clflags

let load_sourcefile ppf sourcefile =
  init_path ();
  let env = initial_env () in
  let fenv = initial_fenv env in
  let str = Pparse.file ppf sourcefile Parse.implementation ast_impl_magic_number in 
  let str = if !Clflags.no_anormal then str else Normalize.normalize_structure str in
  let str = print_if ppf Clflags.dump_parsetree Printast.implementation str in
  let (str, _, env) = type_implementation env str in
    (str, env, fenv)

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
     "-dconstrs", Arg.Set dump_constraints, "print out subframe constraints";
     "-dqexprs", Arg.Set dump_qexprs, "print out all subexpressions with their qualified types";
     "-dqualifs", Arg.Set dump_qualifs, "print out simple qualifiers for all identifiers and integer literals";
     "-dqueries", Arg.Set dump_queries, "print out all theorem prover queries and their results";
     "-dframes", Arg.Set dump_frames, "place frames in an annotation file";
     "-lqueries", Arg.Set log_queries, "log queries to [prover].log";
     "-cqueries", Arg.Set check_queries, "use a backup prover to check all queries";
     "-bquals", Arg.Set brief_quals, "print out the number of refinements for a type instead of their names";
     "-no-simple", Arg.Set no_simple, "do not propagate in simple constraints";
     "-no-simple-subs", Arg.Set no_simple_subs, "do not propagate sets when substitutions are present";
     "-verify-simple", Arg.Set verify_simple, "verify simple constraint propagation against theorem prover result";
     "-use-list", Arg.Set use_list, "use worklist instead of heap in solver";
     "-bprover", Arg.Set always_use_backup_prover, "always use backup prover";
     "-qprover", Arg.Set use_qprover , "use Qprover";
     "-qpdump", Arg.Set qpdump, "dump Qprover queries";
     "-lqualifs", Arg.Set less_qualifs, "generate less qualifiers (lets only under lambdas)";
     "-no-anormal", Arg.Set no_anormal, "don't rewrite the AST for a-normality";
     "-ksimpl", Arg.Set kill_simplify, "kill simplify after a large number of queries to reduce memory usage";
     "-cacheq", Arg.Set cache_queries, "cache theorem prover queries";
     "-psimple", Arg.Set psimple, "prioritize simple constraints";
     "-simpguard", Arg.Set simpguard, "simplify guard (remove iff)";
     "-v", Arg.Int (fun c -> Common.verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output";
     "-verrs", Arg.Set verb_errors, "redacted"
  ] file_argument usage;
  (* nasty obvious hack *)
  let _ = if !dump_qualifs then
    let qs = 
         ["qualif I(v) : v { * * } ^";
          "qualif Int_rel_array_id(v) : Array.length v { * * } ^";
          "qualif Int_rel_bigarray1_id(v) : Bigarray.Array2.dim1 v { * * } ^";
          "qualif Int_rel_bigarray2_id(v) : Bigarray.Array2.dim2 v { * * } ^";
          "qualif Id_rel_id(v) : v { * * } ~A";
          "qualif Id_rel_array_id(v) : ~A { * * } Array.length v";
          "qualif Id_rel_array_idd(v) : v { * * } Array.length ~A";
          "qualif Id_rel_bigarray1_id(v) : ~A { * * } Bigarray.Array2.dim1 v"; 
          "qualif Id_rel_bigarray2_id(v) : ~A { * * } Bigarray.Array2.dim2 v";
          "qualif Id_rel_bigarray1_idd(v) : v { * * } Bigarray.Array2.dim1 ~A"; 
          "qualif Id_rel_bigarray2_idd(v) : v { * * } Bigarray.Array2.dim2 ~A"]
 
    in
    eprintf "@[%s@\n@]" (String.concat "\n" qs); exit 0
  in
  let source = load_sourcefile std_formatter !filename in
  try
    analyze std_formatter !filename source
  with x -> (report_error std_formatter x; exit 1)

let _ = main (); exit 0
