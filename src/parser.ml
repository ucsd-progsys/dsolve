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
  | FORALLTYPE
  | FORALLQUAL
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

open Parsing;;
# 2 "parser.mly"

open Expr
open Type

# 44 "parser.ml"
let yytransl_const = [|
  257 (* ARROW *);
  258 (* BOOL *);
  259 (* BOTTOM *);
  260 (* COLON *);
  261 (* DOT *);
  262 (* ELSE *);
  263 (* EOL *);
  264 (* EQUAL *);
  265 (* FALSE *);
  266 (* FORALLTYPE *);
  267 (* FORALLQUAL *);
  268 (* FUN *);
  269 (* IF *);
  270 (* IN *);
  271 (* INT *);
  273 (* JOIN *);
  274 (* LCURLY *);
  275 (* LET *);
  276 (* LESSEQ *);
  277 (* LPAREN *);
  278 (* LSQUARE *);
  279 (* MEET *);
  281 (* QUESTION *);
  283 (* RCURLY *);
  284 (* RPAREN *);
  285 (* RSQUARE *);
  286 (* THEN *);
  287 (* TOP *);
  288 (* TRUE *);
    0|]

let yytransl_block = [|
  272 (* INTLITERAL *);
  280 (* QLITERAL *);
  282 (* QVAR *);
  289 (* TVAR *);
  290 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\004\000\006\000\006\000\
\006\000\006\000\006\000\006\000\007\000\007\000\007\000\007\000\
\007\000\005\000\005\000\005\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\004\000\004\000\001\000\006\000\001\000\002\000\003\000\
\001\000\001\000\001\000\003\000\003\000\003\000\001\000\001\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\001\000\
\001\000\006\000\008\000\006\000\004\000\006\000\004\000\004\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\023\000\000\000\000\000\000\000\000\000\
\025\000\000\000\000\000\022\000\024\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\019\000\
\020\000\018\000\000\000\010\000\009\000\000\000\015\000\011\000\
\016\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\031\000\000\000\000\000\007\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\012\000\017\000\000\000\000\000\
\013\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\014\000\025\000\046\000\047\000\041\000\048\000\043\000"

let yysindex = "\003\000\
\148\255\000\000\000\000\000\000\232\254\245\254\243\254\009\000\
\000\000\251\254\009\000\000\000\000\000\000\000\165\255\025\255\
\013\255\054\255\182\255\059\255\208\255\011\255\105\255\058\255\
\251\255\009\000\011\255\105\255\009\000\058\255\000\000\000\000\
\000\000\000\000\035\255\000\000\000\000\105\255\000\000\000\000\
\000\000\015\255\073\255\037\255\046\255\064\255\000\000\079\255\
\251\255\069\255\045\255\131\255\077\255\000\000\023\255\017\255\
\105\255\000\000\028\255\028\255\000\000\076\255\066\255\000\000\
\009\000\009\000\009\000\009\000\000\000\000\000\079\255\028\255\
\000\000\000\000\058\255\011\255\251\255\251\255\251\255\234\255\
\250\254\000\000\082\255\009\000\090\255\251\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\209\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\000\000\026\000\030\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\228\255\010\000\235\255\236\255\223\255"

let yytablesize = 333
let yytable = "\015\000\
\035\000\053\000\042\000\001\000\056\000\050\000\019\000\051\000\
\016\000\021\000\059\000\006\000\006\000\032\000\017\000\057\000\
\060\000\055\000\036\000\032\000\018\000\070\000\061\000\057\000\
\049\000\073\000\074\000\052\000\020\000\026\000\032\000\037\000\
\027\000\059\000\033\000\061\000\071\000\038\000\081\000\060\000\
\033\000\034\000\039\000\058\000\070\000\057\000\082\000\034\000\
\072\000\040\000\069\000\033\000\066\000\039\000\083\000\008\000\
\008\000\028\000\034\000\036\000\032\000\054\000\030\000\077\000\
\078\000\079\000\080\000\044\000\045\000\062\000\064\000\063\000\
\037\000\065\000\036\000\032\000\008\000\008\000\038\000\057\000\
\075\000\033\000\086\000\039\000\068\000\076\000\085\000\037\000\
\034\000\059\000\040\000\036\000\032\000\038\000\087\000\060\000\
\033\000\000\000\039\000\000\000\045\000\000\000\000\000\034\000\
\037\000\040\000\036\000\032\000\000\000\000\000\038\000\000\000\
\000\000\033\000\000\000\039\000\000\000\000\000\000\000\037\000\
\034\000\000\000\040\000\000\000\000\000\038\000\000\000\000\000\
\033\000\000\000\039\000\000\000\000\000\000\000\000\000\034\000\
\067\000\040\000\000\000\004\000\005\000\006\000\007\000\008\000\
\000\000\000\000\009\000\000\000\022\000\010\000\000\000\011\000\
\023\000\000\000\003\000\000\000\004\000\005\000\006\000\007\000\
\008\000\000\000\012\000\009\000\013\000\000\000\010\000\000\000\
\011\000\000\000\000\000\000\000\000\000\004\000\005\000\006\000\
\007\000\008\000\000\000\012\000\009\000\013\000\022\000\010\000\
\000\000\011\000\023\000\000\000\000\000\024\000\004\000\005\000\
\006\000\007\000\008\000\000\000\012\000\009\000\013\000\022\000\
\010\000\000\000\011\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\029\000\000\000\012\000\033\000\013\000\
\004\000\005\000\006\000\007\000\008\000\000\000\033\000\009\000\
\000\000\022\000\010\000\000\000\011\000\023\000\000\000\000\000\
\000\000\033\000\000\000\031\000\033\000\000\000\033\000\012\000\
\000\000\013\000\004\000\005\000\006\000\007\000\008\000\084\000\
\000\000\009\000\000\000\022\000\010\000\000\000\011\000\023\000\
\000\000\000\000\000\000\004\000\005\000\006\000\007\000\008\000\
\000\000\012\000\009\000\013\000\022\000\010\000\000\000\011\000\
\023\000\004\000\005\000\006\000\007\000\008\000\029\000\000\000\
\009\000\000\000\012\000\010\000\013\000\011\000\029\000\030\000\
\000\000\000\000\000\000\028\000\000\000\000\000\000\000\030\000\
\012\000\029\000\013\000\028\000\029\000\000\000\029\000\000\000\
\026\000\000\000\030\000\000\000\027\000\030\000\028\000\030\000\
\026\000\028\000\000\000\028\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\026\000\027\000\
\026\000\000\000\027\000\000\000\027\000"

let yycheck = "\001\000\
\022\000\030\000\023\000\001\000\038\000\027\000\008\000\028\000\
\033\001\011\000\017\001\007\001\008\001\003\001\026\001\001\001\
\023\001\038\000\002\001\003\001\034\001\028\001\043\000\001\001\
\026\000\059\000\060\000\029\000\034\001\005\001\003\001\015\001\
\020\001\017\001\024\001\056\000\057\000\021\001\072\000\023\001\
\024\001\031\001\026\001\029\001\028\001\001\001\075\000\031\001\
\021\001\033\001\028\001\024\001\008\001\026\001\076\000\007\001\
\008\001\004\001\031\001\002\001\003\001\027\001\004\001\065\000\
\066\000\067\000\068\000\010\001\011\001\033\001\007\001\026\001\
\015\001\005\001\002\001\003\001\028\001\029\001\021\001\001\001\
\005\001\024\001\084\000\026\001\008\001\020\001\005\001\015\001\
\031\001\017\001\033\001\002\001\003\001\021\001\085\000\023\001\
\024\001\255\255\026\001\255\255\011\001\255\255\255\255\031\001\
\015\001\033\001\002\001\003\001\255\255\255\255\021\001\255\255\
\255\255\024\001\255\255\026\001\255\255\255\255\255\255\015\001\
\031\001\255\255\033\001\255\255\255\255\021\001\255\255\255\255\
\024\001\255\255\026\001\255\255\255\255\255\255\255\255\031\001\
\006\001\033\001\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\255\255\018\001\019\001\255\255\021\001\
\022\001\255\255\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\255\255\032\001\016\001\034\001\255\255\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\032\001\016\001\034\001\018\001\019\001\
\255\255\021\001\022\001\255\255\255\255\025\001\009\001\010\001\
\011\001\012\001\013\001\255\255\032\001\016\001\034\001\018\001\
\019\001\255\255\021\001\022\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\030\001\255\255\032\001\006\001\034\001\
\009\001\010\001\011\001\012\001\013\001\255\255\014\001\016\001\
\255\255\018\001\019\001\255\255\021\001\022\001\255\255\255\255\
\255\255\025\001\255\255\028\001\028\001\255\255\030\001\032\001\
\255\255\034\001\009\001\010\001\011\001\012\001\013\001\014\001\
\255\255\016\001\255\255\018\001\019\001\255\255\021\001\022\001\
\255\255\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\032\001\016\001\034\001\018\001\019\001\255\255\021\001\
\022\001\009\001\010\001\011\001\012\001\013\001\006\001\255\255\
\016\001\255\255\032\001\019\001\034\001\021\001\014\001\006\001\
\255\255\255\255\255\255\006\001\255\255\255\255\255\255\014\001\
\032\001\025\001\034\001\014\001\028\001\255\255\030\001\255\255\
\006\001\255\255\025\001\255\255\006\001\028\001\025\001\030\001\
\014\001\028\001\255\255\030\001\014\001\255\255\255\255\255\255\
\255\255\255\255\255\255\025\001\255\255\255\255\028\001\025\001\
\030\001\255\255\028\001\255\255\030\001"

let yynames_const = "\
  ARROW\000\
  BOOL\000\
  BOTTOM\000\
  COLON\000\
  DOT\000\
  ELSE\000\
  EOL\000\
  EQUAL\000\
  FALSE\000\
  FORALLTYPE\000\
  FORALLQUAL\000\
  FUN\000\
  IF\000\
  IN\000\
  INT\000\
  JOIN\000\
  LCURLY\000\
  LET\000\
  LESSEQ\000\
  LPAREN\000\
  LSQUARE\000\
  MEET\000\
  QUESTION\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  THEN\000\
  TOP\000\
  TRUE\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  QLITERAL\000\
  QVAR\000\
  TVAR\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
      ()
# 279 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tschema) in
    Obj.repr(
# 55 "parser.mly"
                           (
    
    let (e, t) = (_1, _3) in
      if check_type e t then
	Printf.printf("true\n")
      else
	Printf.printf("false\n")
      ;
      flush stdout

  )
# 297 "parser.ml"
               : unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tschema) in
    Obj.repr(
# 69 "parser.mly"
                              ( ForallTyp(_2, _4) )
# 305 "parser.ml"
               : 'tschema))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qschema) in
    Obj.repr(
# 70 "parser.mly"
          ( QSchema(_1) )
# 312 "parser.ml"
               : 'tschema))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'qualliteral) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'qschema) in
    Obj.repr(
# 74 "parser.mly"
                                                 ( ForallQual(_2, _4, _6) )
# 321 "parser.ml"
               : 'qschema))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 75 "parser.mly"
        ( MonoTyp(_1) )
# 328 "parser.ml"
               : 'qschema))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'qual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 79 "parser.mly"
                           (
    match _2 with
	Int(_) -> Int(_1)
      | Bool(_) -> Bool(_1)
      | TyVar(_, a) -> TyVar(_1, a)
      | Arrow(_, t1, t2) -> Arrow(_1, t1, t2)
      | Nil -> Nil
  )
# 343 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mtype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mtype) in
    Obj.repr(
# 87 "parser.mly"
                    ( Arrow(ql Top, _1, _3) )
# 351 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
      ( Int(ql Top) )
# 357 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
       ( Bool(ql Top) )
# 363 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
       ( TyVar(ql Top, _1) )
# 370 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    Obj.repr(
# 91 "parser.mly"
                      ( _2 )
# 377 "parser.ml"
               : 'mtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qual) in
    Obj.repr(
# 95 "parser.mly"
                 ( QualJoin(_1, _3) )
# 385 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qual) in
    Obj.repr(
# 96 "parser.mly"
                 ( QualMeet(_1, _3) )
# 393 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
       ( QualVar(_1) )
# 400 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qualliteral) in
    Obj.repr(
# 98 "parser.mly"
              ( QualLiteral(_1) )
# 407 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'qual) in
    Obj.repr(
# 99 "parser.mly"
                     ( _2 )
# 414 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
      ( Top )
# 420 "parser.ml"
               : 'qualliteral))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
         ( Bottom )
# 426 "parser.ml"
               : 'qualliteral))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
           ( Qual(_1) )
# 433 "parser.ml"
               : 'qualliteral))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 110 "parser.mly"
                    ( _2 )
# 440 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
       ( True )
# 446 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
        ( False )
# 452 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
      ( Var(_1) )
# 459 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
             ( Num(_1) )
# 466 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 115 "parser.mly"
                           ( If(_2, _4, _6) )
# 475 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'tschema) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 116 "parser.mly"
                                         ( Let(_2, _4, _6, _8) )
# 485 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'mtype) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 117 "parser.mly"
                                ( Abs(_2, _4, _6) )
# 494 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 118 "parser.mly"
                          ( TyAbs(_2, _4) )
# 502 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'qualliteral) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 119 "parser.mly"
                                             ( QualAbs(_2, _4, _6) )
# 511 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'mtype) in
    Obj.repr(
# 120 "parser.mly"
                            ( TyApp(_1, _3) )
# 519 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'qualliteral) in
    Obj.repr(
# 121 "parser.mly"
                                ( QualApp(_1, _3) )
# 527 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 122 "parser.mly"
          ( App(_1, _2) )
# 535 "parser.ml"
               : 'exp))
(* Entry query *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let query (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
;;
