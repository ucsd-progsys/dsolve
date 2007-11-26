open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error ->
      fprintf ppf "Preprocessor error"
  | Env.Error err ->
      Env.report_error ppf err
  | Ctype.Tags(l, l') -> fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, err) ->
      Location.print ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, err) ->
      Location.print ppf loc; Typeclass.report_error ppf err
  | Includemod.Error err ->
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print ppf loc; Typemod.report_error ppf err
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn

