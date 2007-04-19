type token =
  | AND
  | ARROW
  | BANG
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
  | LESS
  | LESSEQ
  | LET
  | LPAREN
  | LSQUARE
  | MEET
  | MINUS
  | NEQUAL
  | NOT
  | OR
  | PLUS
  | PRED
  | QLITERAL of (string)
  | QUESTION
  | QVAR of (string)
  | RCURLY
  | RPAREN
  | RSQUARE
  | THEN
  | TIMES
  | TOP
  | TRUE
  | TVAR of (string)
  | VAR of (string)

val query :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
