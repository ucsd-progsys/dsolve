%{

open Expr
open Type

%}


%token ARROW
%token BOOL
%token BOTTOM
%token COLON
%token DOT
%token ELSE
%token EOL
%token EQUAL
%token FALSE
%token FORALLTYPE
%token FORALLQUAL
%token FUN
%token IF
%token IN
%token INT
%token <int> INTLITERAL
%token JOIN
%token LCURLY
%token LET
%token LESSEQ
%token LPAREN
%token LSQUARE
%token MEET
%token <string> QLITERAL
%token QUESTION
%token <string> QVAR
%token RCURLY
%token RPAREN
%token RSQUARE
%token THEN
%token TOP
%token TRUE
%token <string> TVAR
%token <string> VAR

%right ARROW
%left  MEET JOIN
%nonassoc QUALIFY

%type <unit> query
%start query


%%
query:
  EOL {}
| exp QUESTION tschema EOL {
    
    let (e, t) = ($1, $3) in
      if check_type e t then
	Printf.printf("true\n")
      else
	Printf.printf("false\n")
      ;
      flush stdout

  }
;

tschema:
  FORALLTYPE TVAR DOT tschema { ForallTyp($2, $4) }
| qschema { QSchema($1) }
;

qschema:
  FORALLQUAL QVAR LESSEQ qualliteral DOT qschema { ForallQual($2, $4, $6) }
| mtype { MonoTyp($1) }
;

mtype:
  qual mtype %prec QUALIFY {
    match $2 with
	Int(_) -> Int($1)
      | Bool(_) -> Bool($1)
      | TyVar(_, a) -> TyVar($1, a)
      | Arrow(_, t1, t2) -> Arrow($1, t1, t2)
      | Nil -> Nil
  }
| mtype ARROW mtype { Arrow(ql Top, $1, $3) }
| INT { Int(ql Top) }
| BOOL { Bool(ql Top) }
| TVAR { TyVar(ql Top, $1) }
| LPAREN mtype RPAREN { $2 }
;

qual:
  qual JOIN qual { QualJoin($1, $3) }
| qual MEET qual { QualMeet($1, $3) }
| QVAR { QualVar($1) }
| qualliteral { QualLiteral($1) }
| LPAREN qual RPAREN { $2 }
;

qualliteral:
  TOP { Top }
| BOTTOM { Bottom }
| QLITERAL { Qual($1) }
;


exp:
  LPAREN exp RPAREN { $2 }
| TRUE { True }
| FALSE { False }
| VAR { Var($1) }
| INTLITERAL { Num($1) }
| IF exp THEN exp ELSE exp { If($2, $4, $6) }
| LET VAR COLON tschema EQUAL exp IN exp { Let($2, $4, $6, $8) }
| FUN VAR COLON mtype EQUAL exp { Abs($2, $4, $6) }
| FORALLTYPE TVAR DOT exp { TyAbs($2, $4) }
| FORALLQUAL QVAR LESSEQ qualliteral DOT exp { QualAbs($2, $4, $6) }
| exp LSQUARE mtype RSQUARE { TyApp($1, $3) }
| exp LCURLY qualliteral RCURLY { QualApp($1, $3) }
| exp exp { App($1, $2) }
%%
