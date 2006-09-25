type token =
  | ARROW
  | BOOL
  | BOTTOM
  | COLON
  | DOT
  | ELSE
  | EOL
  | EQUAL
  | FALSE
  | FUN
  | IF
  | IN
  | INT
  | INTLITERAL of (int)
  | JOIN
  | LCURLY
  | LET
  | LESSEQ
  | LPAREN
  | LSQUARE
  | MEET
  | QLITERAL of (string)
  | QUESTION
  | QVAR of (string)
  | RCURLY
  | RPAREN
  | RSQUARE
  | THEN
  | TOP
  | TRUE
  | TVAR of (string)
  | VAR of (string)

val query :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
