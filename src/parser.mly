%{

open Expr
open Type
open Check
open Infer
open Predicate


let next_id = ref 0
let get_next_expr_id () =
  incr next_id;
  string_of_int !next_id


let active_quals = ref Builtins.quals


let make_binapp f x y =
  App(App(ExpVar(f, get_next_expr_id()), x, get_next_expr_id()),
      y, get_next_expr_id())


%}


%token AND
%token ARROW
%token BANG
%token BOOL
%token BOTTOM
%token COLON
%token COMMA
%token DOT
%token ELSE
%token EOL
%token EQUAL
%token FALSE
%token FIX
%token FUN
%token IF
%token IN
%token INT
%token <int> INTLITERAL
%token JOIN
%token LCURLY
%token LESS
%token LESSEQ
%token LET
%token LETREC
%token LPAREN
%token LSQUARE
%token MATCH
%token MEET
%token MINUS
%token NEQUAL
%token NOT
%token OR
%token PLUS
%token <string> QLITERAL
%token QUAL
%token QUESTION
%token <string> QVAR
%token RCURLY
%token RPAREN
%token RSQUARE
%token <string> TAG 
%token THEN
%token TIMES
%token TOP
%token TRUE
%token TYPE
%token <string> TVAR
%token <string> VAR
%token WITH

%right ARROW
%left  MEET JOIN
%left  ARG
%right LESS LESSEQ EQ
%left  PLUS MINUS
%right TIMES
%right QUALIFY

%type <unit> query
%start query


%%
query:
  EOL {}
| error EOL {

    (* Dig the FORTRAN-style error message. *)
    Printf.printf "> SYNTAX ERROR\n\n";
    flush stdout
}
| exp QUESTION mtype EOL {

    let (e, t) = ($1, $3) in
      Printf.printf ">> ";
      if check_type !active_quals e t then
	Printf.printf "OK"
      else
	Printf.printf "NO"
      ;
      Printf.printf "\n\n";
      flush stdout

  }
| QUESTION exp EOL {

    let e = $2 in
      begin try
	let typemap = infer_types e !active_quals in
        let annotator e = pprint_type (typemap e) in
	  Printf.printf ">> %s\n\n" (pprint_type (typemap e));
          Printf.printf "%s" (pprint_annotated_expr annotator 0 e)
      with _ ->
	Printf.printf "Cannot infer type"
      end;
      Printf.printf "\n\n";
      flush stdout
  }
| exp EOL {

    let e = $1 in
      try
	let (v, t) = (eval e, Int []) in
	  Printf.printf "> %s: %s\n\n" (pprint_value v) (pprint_type t);
	  flush stdout
      with _ ->
	Printf.printf "Cannot evaluate expression\n\n";
	flush stdout	  
  }
 | QUAL QLITERAL LPAREN VAR RPAREN COLON pred EOL {

    active_quals := ($2, PredOver($4, $7))::(!active_quals);
    let pprint_param_pred = function
	(name, PredOver(x, p)) ->
	  name ^ "(" ^ x ^ "): " ^ pprint_predicate p
    in
    let activestrs = List.map pprint_param_pred !active_quals in
      Printf.printf "%s\n\n" (Misc.join activestrs "\n");
      flush stdout
  }
;

mtype:
| mtype COLON VAR ARROW mtype { Arrow($3, $1, $5) }
| qualifier_list INT { Int $1 }
| TVAR { TyVar $1 }
| LPAREN mtype RPAREN { $2 }
;

qualifier_list:
  qualifier { [$1] }
| qualifier qualifier_list { $1::$2 }

qualifier:
  QLITERAL { ($1, List.assoc $1 !active_quals) }
;

exp:
  simple_exp param_list { List.fold_left (fun f g -> App(f, g, get_next_expr_id())) $1 $2 }
| simple_exp { $1 }
;

simple_exp:
  LPAREN exp RPAREN { $2 }
| VAR { ExpVar($1, get_next_expr_id()) }
| INTLITERAL { Num($1, get_next_expr_id()) }
| exp PLUS exp { make_binapp "+" $1 $3 }
| exp MINUS exp { make_binapp "-" $1 $3 }
| exp EQUAL exp { make_binapp "=" $1 $3 }
| exp NEQUAL exp { make_binapp "!=" $1 $3 }
| exp LESS exp { make_binapp "<" $1 $3 }
| exp LESSEQ exp { make_binapp "<=" $1 $3 }
| IF exp THEN exp ELSE exp { If($2, $4, $6, get_next_expr_id()) }
| LET VAR COLON mtype EQUAL exp IN exp { Let($2, Some $4, $6, $8, get_next_expr_id()) }
| LET VAR EQUAL exp IN exp { Let($2, None, $4, $6, get_next_expr_id()) }
| LETREC VAR EQUAL FUN VAR ARROW exp IN exp {
    let expf = Abs($5, None, $7, get_next_expr_id()) in
      LetRec($2, None, expf, $9, get_next_expr_id())
  }
| FUN VAR COLON mtype ARROW exp { Abs($2, Some $4, $6, get_next_expr_id()) }
| FUN VAR ARROW exp { Abs($2, None, $4, get_next_expr_id()) }
| LPAREN mtype BANG mtype RPAREN exp { Cast($2, $4, $6, get_next_expr_id()) }
;

param_list:
  simple_exp param_list { $1::$2 }
| simple_exp { [$1] }
;

exp_list:
  exp COMMA exp_list { $1::$3 }
| exp { [$1] }
;

predexp:
  INTLITERAL { Predicate.PInt($1) }
| LPAREN predexp RPAREN { $2 }
| VAR { Predicate.Var($1) }
| predexp PLUS predexp { Predicate.Binop($1, Plus, $3) }
| predexp MINUS predexp { Predicate.Binop($1, Minus, $3) }
| predexp TIMES predexp { Predicate.Binop($1, Times, $3) }
;

pred:
  TRUE { Predicate.True }
| LPAREN pred RPAREN { $2 }
| predexp EQUAL predexp { Predicate.Atom($1, Eq, $3) }
| predexp NEQUAL predexp { Predicate.Atom($1, Ne, $3) }
| predexp LESS predexp { Predicate.Atom($1, Lt, $3) }
| predexp LESSEQ predexp { Predicate.Atom($1, Le, $3) }
| NOT pred { Predicate.Not($2) }
| pred AND pred { Predicate.And($1, $3) }
| pred OR pred { Predicate.Or($1, $3) }
;

%%
