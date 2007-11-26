open Clflags
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

let initial_fenv env =
  Lightenv.addn (Builtins.frames env) Lightenv.empty

let (++) x f = f x

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let analyze ppf sourcefile =
  init_path ();
  let env = initial_env () in
  let _ = initial_fenv env in
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
    ignore(
      Pparse.file ppf sourcefile Parse.implementation ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ Typemod.type_implementation sourcefile ".cmo" modulename env)

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
     "-dframes", Arg.String (fun s -> Clflags.dump_frames := Some s), "place frames in an annotation file";
     "-lqueries", Arg.Set log_queries, "log queries to [prover].log";
     "-cqueries", Arg.Set check_queries, "use a backup prover to check all queries";
     "-bquals", Arg.Set brief_quals, "print out the number of refinements for a type instead of their names";
     "-no-simple", Arg.Set no_simple, "do not propagate in simple constraints";
     "-no-simple-subs", Arg.Set no_simple_subs, "do not propagate sets when substitutions are present";
     "-verify-simple", Arg.Set verify_simple, "verify simple constraint propagation against theorem prover result";
     "-use-list", Arg.Set use_list, "use worklist instead of heap in solver";
     "-bprover", Arg.Set always_use_backup_prover, "always use backup prover";
     "-lqualifs", Arg.Set less_qualifs, "generate less qualifiers (lets only under lambdas)";
     "-anormal", Arg.Set make_anormal, "rewrite the AST for a-normality";
     "-ksimpl", Arg.Set kill_simplify, "kill simplify after a large number of queries to reduce memory usage";
     "-cacheq", Arg.Set cache_queries, "cache theorem prover queries"
  ] file_argument usage;
  try analyze std_formatter !filename
  with x -> report_error err_formatter x

let _ = main ()
