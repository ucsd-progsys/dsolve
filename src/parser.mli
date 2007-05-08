type token =
  | AND
  | ARROW
  | BANG
  | BOOL
  | BOTTOM
  | COLON
  | COMMA
  | DOT
  | ELSE
  | EOL
  | EQUAL
  | FALSE
  | FIX
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
  | MATCH
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
  | TAG of (string)
  | THEN
  | TIMES
  | TOP
  | TRUE
  | TYPE
  | TVAR of (string)
  | VAR of (string)
  | WITH

val query :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
