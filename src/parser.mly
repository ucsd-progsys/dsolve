%{

open Toplevel
open TheoremProver
open Qualgraph
open Flowgraph
open Predicate
open Annotate
open Expr
open Type


let print_qualmap qm =
  let print_labelled_qual = function
      QualFrom(q, None) -> Printf.printf "%s, " q
    | QualFrom(q, Some e) ->
	let site_str = match FlowGraph.E.label e with
	    None -> ""
	  | Some(Call i) -> "(" ^ string_of_int i ^ " "
	  | Some(Return i) -> ")" ^ string_of_int i ^ " "
	in
	  Printf.printf "%s%s, " site_str q
  in
  let print_kv k v =
    Printf.printf "%s: " (string_of_vlabel (FlowGraph.V.label k));
    LabelledQualSet.iter print_labelled_qual v;
    Printf.printf "\n"
  in
    QualMap.iter print_kv qm;
    Printf.printf "\n"


let next_id = ref 0
let get_next_expr_id () =
  incr next_id;
  string_of_int !next_id


let active_preds = ref []


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
%token LPAREN
%token LSQUARE
%token MATCH
%token MEET
%token MINUS
%token NEQUAL
%token NOT
%token OR
%token PLUS
%token PRED
%token <string> QLITERAL
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
%left  PLUS MINUS
%right TIMES
%right QUALIFY

%type <unit> query
%start query


%%
query:
  EOL {}
| exp QUESTION tschema EOL {
    
    let (e, t) = ($1, $3) in
      Printf.printf ">> ";
      if check_type e t then
	Printf.printf "true"
      else
	Printf.printf "false"
      ;
      Printf.printf "\n\n";
      flush stdout

  }
| TYPE exp EOL {

    let e = $2 in
      Printf.printf(">> ");
      begin try
	let t = infer_type e in
	  Printf.printf "%s" (pprint_type t)
      with _ ->
	Printf.printf "Cannot infer type"
      end;
      Printf.printf "\n\n";
      flush stdout
  }
| BANG exp EOL {

    let e = $2 in
      Printf.printf(">> Graph:\n");
      let (graph, qm) = expr_qualgraph e in
	FlowGraphPrinter.output_graph stdout graph;
	Printf.printf "\n<< EOG\n\n";
	Printf.printf "Initial qualmap:\n";
	print_qualmap qm;
	let qm' = propagate_vertex_qualifiers graph qm in
	  Printf.printf "Final qualmap:\n";
	  print_qualmap qm';
	  flush stdout
  }
| exp EOL {

    let e = $1 in
      try
	let (v, t) = (eval e, Int(Top)) in
	  Printf.printf "> %s: %s\n\n" (pprint_value v) (pprint_type t);
	  flush stdout
      with _ ->
	Printf.printf "Cannot evaluate expression\n\n";
	flush stdout	  
  }
| PRED QLITERAL LPAREN VAR RPAREN COLON pred EOL {

    active_preds := ($2, PredOver($4, $7))::(!active_preds);
    let pprint_param_pred = function
	(name, PredOver(x, p)) ->
	  name ^ "(" ^ x ^ "): " ^ pprint_predicate p
    in
    let activestrs = List.map pprint_param_pred !active_preds in
    let active_predstr = Misc.join activestrs "\n" in
      Printf.printf "%s\n\n" active_predstr;
      flush stdout
  }
| QUESTION exp EOL {

    let exp = $2 in
    let exp_pred = expr_predicate exp in
      Prover.push(exp_pred);
      let qmap = fixedpoint_annotate exp !active_preds in
	Printf.printf "%s\n\n" (pprint_annotated_expr (expr_quals qmap) exp);
	Prover.reset();
	flush stdout
  }
;

tschema:
  TVAR DOT tschema { ForallTyp($1, $3) }
| qschema { $1 }
;

qschema:
  QVAR LESSEQ qualliteral DOT qschema { ForallQual($1, $3, $5) }
| mtype { $1 }
;

mtype:
  qual mtype %prec QUALIFY {
    match $2 with
	Int(_) -> Int($1)
      | Bool(_) -> Bool($1)
      | TyVar(_, a) -> TyVar($1, a)
      | Arrow(_, t1, t2) -> Arrow($1, t1, t2)
      | Nil -> Nil
      | t -> t
  }
| mtype ARROW mtype { Arrow(Top, $1, $3) }
| INT { Int(Top) }
| BOOL { Bool(Top) }
| TVAR { TyVar(Top, $1) }
| LPAREN mtype RPAREN { $2 }
;

qual:
  qual JOIN qual { QualJoin($1, $3) }
| qual MEET qual { QualMeet($1, $3) }
| QVAR { QualVar($1) }
| qualliteral { $1 }
| LPAREN qual RPAREN { $2 }
;

qualliteral:
  TOP { Top }
| BOTTOM { Bottom }
| QLITERAL { Qual($1) }
;


exp:
  LPAREN exp RPAREN { $2 }
| TRUE { TrueExp(get_next_expr_id()) }
| FALSE { FalseExp(get_next_expr_id()) }
| VAR { ExpVar($1, get_next_expr_id()) }
| INTLITERAL { Num($1, get_next_expr_id()) }
| TAG LPAREN exp_list RPAREN { TyCon($1, $3, get_next_expr_id()) }
| exp PLUS exp { BinOp(Plus, $1, $3, get_next_expr_id()) }
| exp MINUS exp { BinOp(Minus, $1, $3, get_next_expr_id()) }
| exp TIMES exp { BinOp(Times, $1, $3, get_next_expr_id()) }
| exp EQUAL exp { BinRel(Eq, $1, $3, get_next_expr_id()) }
| exp NEQUAL exp { BinRel(Ne, $1, $3, get_next_expr_id()) }
| exp LESS exp { BinRel(Lt, $1, $3, get_next_expr_id()) }
| exp LESSEQ exp { BinRel(Le, $1, $3, get_next_expr_id()) }
| IF exp THEN exp ELSE exp { If($2, $4, $6, get_next_expr_id()) }
| MATCH exp WITH pattern_expr_list { Match($2, $4, get_next_expr_id()) }
| LET VAR COLON tschema EQUAL exp IN exp { Let($2, Some $4, $6, $8, get_next_expr_id()) }
| LET VAR EQUAL exp IN exp { Let($2, None, $4, $6, get_next_expr_id()) }
| FUN VAR COLON mtype EQUAL exp { Abs($2, Some $4, $6, get_next_expr_id()) }
| FUN VAR EQUAL exp { Abs($2, None, $4, get_next_expr_id()) }
| FIX VAR DOT exp { Fix($2, $4, get_next_expr_id()) }
| TVAR DOT exp { TyAbs($1, $3, get_next_expr_id()) }
| QVAR LESSEQ qualliteral DOT exp { QualAbs($1, $3, $5, get_next_expr_id()) }
| LCURLY LSQUARE qual RSQUARE RCURLY exp { Annot($3, $6, get_next_expr_id()) }
| exp LSQUARE mtype RSQUARE { TyApp($1, $3, get_next_expr_id()) }
| exp LCURLY qualliteral RCURLY { QualApp($1, $3, get_next_expr_id()) }
| exp exp { App($1, $2, get_next_expr_id()) }
;


exp_list:
  exp COMMA exp_list { $1::$3 }
| exp { [$1] }
;


pattern_expr_list:
  pattern_expr JOIN pattern_expr_list { $1::$3 }
| pattern_expr { [$1] }
;


pattern_expr:
  pattern ARROW exp { ($1, $3) }
;


pattern:
  TAG { PatternTyCon($1, []) }
| TAG LPAREN pattern_args RPAREN { PatternTyCon($1, $3) }
| VAR { PatternVar($1) }
;


pattern_args:
  pattern COMMA pattern_args { $1::$3 }
| pattern { [$1] }
;


predexp:
  INTLITERAL { Predicate.Int($1) }
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
