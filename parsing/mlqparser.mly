/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id: parser.mly,v 1.126 2006/12/15 04:50:30 garrigue Exp $ */

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree

let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mkqpat d =
  { pqual_pat_desc = d; pqual_pat_loc = symbol_rloc() }
let mkpredpat d =
  { ppredpat_desc = d; ppredpat_loc = symbol_rloc() }
let mkpredpatexp d =
  { ppredpatexp_desc = d; ppredpatexp_loc = symbol_rloc() }
let mkpredpatorexp_exp e =
  { ppredpatorexp_desc = Ppredexp e; ppredpatorexp_loc = symbol_rloc() }
let mkpredpatorexp_pred p =
  { ppredpatorexp_desc = Ppredpat p; ppredpatorexp_loc = symbol_rloc() }
let mkfield d =
  { pfield_desc = d; pfield_loc = symbol_rloc() }

(* Convenience for liquid interfaces *)

let rw_frame f nr =
    match f with
      PFvar(s, b, r) -> PFvar(s, b, nr)
    | PFrec(s, rr, _) -> PFrec(s, rr, nr)
    | PFsum(l, rro, cs, _) -> PFsum(l, rro, cs, nr)
    | PFconstr(s, f, i, r) -> PFconstr(s, f, i, nr)
    | PFarrow _ -> assert false
    | PFtuple(a, r) -> PFtuple(a, nr)
    | PFrecord(a, r) -> PFrecord(a, nr)

let inj_name s = function
    | PFconstr (a, b, _, r) -> PFconstr (a, b, Some s, r)
    | f -> f

let ptrue = ("", {ppredpat_desc = Ppredpat_true; 
                  ppredpat_loc = Location.none})

let mk_implies a b =
  let pre = mkpredpat (Ppredpat_not(a)) in
    mkpredpat (Ppredpat_or(pre, b))

let mkconstr a b r = PFconstr (a, b, None, r)
let mksum p rro cs r = PFsum (p, rro, cs, r)
let mkvar a b r = PFvar (a, b, r)
let mkrecvar a rr r = PFrec (a, rr, r)
let mkarrow v a b = PFarrow (v, a, b)
let mktuple a r = PFtuple (a, r)
let mkrecord a r = PFrecord (a, r)
let mktrue_constr a b = mkconstr a b ptrue
let mktrue_sum p rro cs = mksum p rro cs ptrue
let mktrue_var a b = mkvar a b ptrue
let mktrue_recvar a rr = mkrecvar a rr ptrue
let mktrue_tuple a = mktuple a ptrue
let mktrue_record a = mkrecord a ptrue

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token BACKQUOTE
%token BAR
%token BARBAR
%token BARRBRACKET
%token <char> CHAR
%token COLON
%token COLONCOLON
%token COMMA
%token DOT
%token DOTDOT
%token ELSE
%token EOF
%token EQUAL
%token FALSE
%token <string> FLOAT
%token FOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token <string> LIDENT
%token LPAREN
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token EQUALGREATER
%token MUTABLE
%token <nativeint> NATIVEINT
%token OF
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PLUS
%token <string> PREFIXOP
%token QUALIF
%token INTS
%token MODULE_DEPENDENCY
%token EMBED
%token PREDICATE
%token MEASURE
%token REFINEMENT
%token QUESTION
%token QUESTIONQUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token STAR
%token <string> STRING
%token THEN
%token TILDE
%token TRUE
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token UNINTERPRETED
%token FORALL
%token EXISTS
%token AXIOM
%token IFF
%token VAL
%token NON_REFINED_VAL

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%nonassoc below_BARBAR
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS MINUS MINUSDOT  /* expr (e OP e OP e) */
%nonassoc below_STAR
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus              /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
%nonassoc below_IDENT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NATIVEINT PREFIXOP STRING TRUE UIDENT


/* Entry points */

%start qualifier_patterns               /* pattern qualifier files */
%type <string list * int list * Parsetree.qualifier_declaration list> qualifier_patterns
%start liquid_interface                 /* for mlq refined interface files */
%type <Parsetree.liquid_sig> liquid_interface

%%

/* Entry points */

liquid_interface:
  | liquid_signature EOF   { $1 }
  | EOF                    { [] }
;

/* Polymorphic types */

typevar_list:
        QUOTE ident                             { [$2] }
      | typevar_list QUOTE ident                { $3 :: $1 }
;
poly_type:
        core_type
          { mktyp(Ptyp_poly([], $1)) }
      | typevar_list DOT core_type
          { mktyp(Ptyp_poly(List.rev $1, $3)) }
;

/* Core types */

core_type:
    core_type2
      { $1 }
  | core_type2 AS QUOTE ident
      { mktyp(Ptyp_alias($1, $4)) }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $2 ,
               {ptyp_desc = Ptyp_constr(Lident "option", [$4]);
                ptyp_loc = $4.ptyp_loc}, $6)) }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $1 ,
               {ptyp_desc = Ptyp_constr(Lident "option", [$2]);
                ptyp_loc = $2.ptyp_loc}, $4)) }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow($1, $3, $5)) }
  | core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("", $1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mktyp(Ptyp_constr($4, List.rev $2)) }
  | LESS meth_list GREATER
      { mktyp(Ptyp_object $2) }
  | LESS GREATER
      { mktyp(Ptyp_object []) }
  | SHARP class_longident opt_present
      { mktyp(Ptyp_class($2, [], $3)) }
  | simple_core_type2 SHARP class_longident opt_present
      { mktyp(Ptyp_class($3, [$1], $4)) }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident opt_present
      { mktyp(Ptyp_class($5, List.rev $2, $6)) }
  | LBRACKET tag_field RBRACKET
      { mktyp(Ptyp_variant([$2], true, None)) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type2 RBRACKET
      { mktyp(Ptyp_variant([$2], true, None)) }
*/
  | LBRACKET BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, None)) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant($2 :: List.rev $4, true, None)) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, false, None)) }
  | LBRACKETGREATER RBRACKET
      { mktyp(Ptyp_variant([], false, None)) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, Some [])) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, true, Some (List.rev $5))) }
;
row_field_list:
    row_field                                   { [$1] }
  | row_field_list BAR row_field                { $3 :: $1 }
;
row_field:
    tag_field                                   { $1 }
  | simple_core_type2                           { Rinherit $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list
      { Rtag ($1, $3, List.rev $4) }
  | name_tag
      { Rtag ($1, true, []) }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
amper_type_list:
    core_type                                   { [$1] }
  | amper_type_list AMPERSAND core_type         { $3 :: $1 }
;
opt_present:
    LBRACKETGREATER name_tag_list RBRACKET      { List.rev $2 }
  | /* empty */                                 { [] }
;
name_tag_list:
    name_tag                                    { [$1] }
  | name_tag_list name_tag                      { $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
meth_list:
    field SEMI meth_list                        { $1 :: $3 }
  | field opt_semi                              { [$1] }
  | DOTDOT                                      { [mkfield Pfield_var] }
;
field:
    label COLON poly_type                       { mkfield(Pfield($1, $3)) }
;
label:
    LIDENT                                      { $1 }
;


/* Qualifiers */

qualifier_pattern_list:
    /* empty */ 
      { [] }
  | QUALIF qualifier_pattern_declaration qualifier_pattern_list
      { $2::$3 }

dep_list_opt:
    /* empty */
      { [] }
  | dep_list
      { $1 }

dep_list:
    MODULE_DEPENDENCY UIDENT
      { [$2] }
  | MODULE_DEPENDENCY UIDENT dep_list
      { $2 :: $3 }

int_list_opt:
    /* empty */
      { [] }
  | INTS qual_intlist
      { $2 }

qualifier_patterns:
    dep_list_opt int_list_opt qualifier_pattern_list EOF
      { ($1, $2, $3) }

qualifier_pattern_declaration:
    UIDENT LPAREN LIDENT RPAREN LPAREN qual_ty_anno RPAREN COLON qualifier_pattern  
    { ($1, mkqpat($3, $6, $9)) }
  | UIDENT LPAREN LIDENT RPAREN COLON qualifier_pattern
    { ($1, mkqpat($3, [], $6)) }

qual_ty_anno:
    UIDENT COLON simple_core_type_or_tuple
    { [($1, $3)] } 
  | UIDENT COLON simple_core_type_or_tuple COMMA qual_ty_anno
    { ($1, $3)::$5 }

quant_id_list:
    LIDENT COLON prover_type                      { [($1, $3)] }
  | LIDENT COLON prover_type COMMA quant_id_list  { ($1, $3) :: $5 }

qualifier_pattern:
    TRUE                                    { mkpredpat Ppredpat_true }                    
  | qualifier_pattern AND qualifier_pattern { mkpredpat (Ppredpat_and($1, $3)) }
  | qualifier_pattern OR qualifier_pattern  { mkpredpat (Ppredpat_or($1, $3)) }
  | qualifier_pattern EQUALGREATER qualifier_pattern { mkpredpat (Ppredpat_implies($1, $3)) }
  | LPAREN qualifier_pattern MINUSGREATER qualifier_pattern RPAREN
      { mkpredpat (Ppredpat_implies($2, $4)) }
  | MINUSDOT qualifier_pattern              { mkpredpat (Ppredpat_not($2)) }              
  | LPAREN qualifier_pattern RPAREN         { $2 }
  | LPAREN qualifier_pattern IFF qualifier_pattern RPAREN
      { mkpredpat (Ppredpat_iff($2, $4)) } 
  | qual_expr qual_rel qual_expr            
      { mkpredpat (Ppredpat_atom($1, $2, $3)) }
  | FORALL LPAREN quant_id_list DOT qualifier_pattern RPAREN  { mkpredpat (Ppredpat_forall($3, $5)) }
  | EXISTS LPAREN quant_id_list DOT qualifier_pattern RPAREN  { mkpredpat (Ppredpat_exists($3, $5)) }
  | QUESTION qual_expr                                        { mkpredpat (Ppredpat_boolexp($2)) } 


qual_rel:
    qual_lit_rel                            { [$1] }
  | LBRACE qual_rel_list RBRACE             { $2 }
  | LBRACE STAR STAR RBRACE                 
    { [] }
  
qual_lit_rel:
    INFIXOP0                
    {   if $1 = "<=" then Pred_le
        else if $1 = "!=" then Pred_ne
				else if $1 = ">=" then Pred_ge
        else if $1 = "=" then Pred_eq
        else if $1 = "<" then Pred_lt
        else if $1 = ">" then Pred_gt
        else raise Parse_error
    }
  | EQUAL                                   { Pred_eq }
  | GREATER                                 { Pred_gt }
  | LESS                                    { Pred_lt }

qual_rel_list:
    qual_lit_rel                            { [$1] }
  | qual_lit_rel COMMA qual_rel_list        { $1::$3 }

qual_expr:
    qual_expr qual_op qual_expr_1           
    { mkpredpatexp (Ppredpatexp_binop($1, $2, $3)) }
  | qual_expr_1                             { $1 }

qual_expr_1: 
    qual_litident qual_term_list 
    { mkpredpatexp (Ppredpatexp_funapp(Longident.parse $1, $2)) }
  | qual_term COLONCOLON qual_term
    { mkpredpatexp (Ppredpatexp_funapp(Longident.parse "::", [$1; $3])) }
  | COLONCOLON qual_term qual_term
    { mkpredpatexp (Ppredpatexp_funapp(Longident.parse "::", [$2; $3])) }
  | UIDENT qual_term_list
    { mkpredpatexp (Ppredpatexp_funapp(Longident.parse $1, $2)) } 
  | UIDENT
    { mkpredpatexp (Ppredpatexp_var([Longident.parse $1])) }

  | qual_term                               { $1 }

qual_term:
    LPAREN qual_expr RPAREN                 { $2 }
  | qual_litident /* literal */
    { mkpredpatexp (Ppredpatexp_var([Longident.parse $1])) } 
  | LBRACKET qual_litident_list RBRACKET
    { mkpredpatexp (Ppredpatexp_var($2)) }
  | TILDE UIDENT /* var */
    { mkpredpatexp (Ppredpatexp_mvar($2)) } 
  | INT
    { mkpredpatexp (Ppredpatexp_int([$1])) }
  | INFIXOP1  /* wild int @ */
    { mkpredpatexp (Ppredpatexp_any_int) }
  | LBRACKET qual_intlist RBRACKET
    { mkpredpatexp (Ppredpatexp_int($2)) }
  | qual_term DOT LIDENT                
    { mkpredpatexp (Ppredpatexp_field($3, $1)) }
  | LPAREN qualifier_pattern QUESTION qual_expr COLON 
    qual_expr RPAREN
    { mkpredpatexp (Ppredpatexp_ite($2, $4, $6)) }
  | IF LPAREN qualifier_pattern RPAREN THEN LPAREN qual_expr RPAREN ELSE 
    LPAREN qual_expr RPAREN
    { mkpredpatexp (Ppredpatexp_ite($3, $7, $11)) }

qual_intlist:
    INT                                     { [$1] }
  | MINUS INT                               { [0-$2] }
  | INT COMMA qual_intlist                  { $1::$3 }
  | MINUS INT COMMA qual_intlist            { (0-$2)::$4 }

qual_litident:
    UIDENT DOT qual_litident                { $1 ^ "." ^ $3 }
  | LBRACKET constr_ident RBRACKET          { $2 }
  | LIDENT                                  { $1 }

qual_litident_list:
    qual_litident COMMA qual_litident_list  { (Longident.parse $1) :: $3 }
  | qual_litident                           { [(Longident.parse $1)] }

qual_term_list:
  | qual_term                               { [$1] }
  | qual_term qual_term_list                { $1::$2 }

qual_op:
    qual_lit_op                                { [$1]  }
  | LBRACELESS qual_lit_op_list GREATERRBRACE  { $2 }
  | LBRACELESS STAR STAR GREATERRBRACE         
    { [] }

qual_lit_op:
    PLUS                                    { Predexp_plus }
  | MINUS                                   { Predexp_minus }    
  | STAR                                    { Predexp_times }  
  | INFIXOP3                                
    {  match $1 with 
			  "/" -> Predexp_div 
      | "*" -> Predexp_times
      | _ -> raise Parse_error }

qual_lit_op_list:
    qual_lit_op                             { [$1] }
  | qual_lit_op COMMA qual_lit_op_list      { $1::$3 }

/* Liquid signatures */

liquid_signature:
    liquid_decl liquid_signature        { $1 :: $2 }
  | liquid_decl                         { [$1] }

liquid_decl:
    liquid_val_decl                     { let (name, decl) = $1 in LvalDecl(name, decl) }
  | liquid_nrval_decl                   { let (name, decl) = $1 in LnrvalDecl(name, decl) }
  | liquid_measure_decl                 { let (name, decl) = $1 in LmeasDecl(name, decl) }
  | liquid_uninterpreted_decl           { let (name, ty)   = $1 in LunintDecl(name, ty) }
  | liquid_type_embedding               { let (ty, str)    = $1 in LembedDecl(ty, str) }
  | liquid_axiom_decl                   { let (name, pred) = $1 in LaxiomDecl(name, pred) }
  | liquid_recref_decl                  { LrecrefDecl }

liquid_val_decl:
    VAL val_longident COLON liquid_type           
      { (String.concat "." (Longident.flatten $2), $4) }

liquid_nrval_decl:
    NON_REFINED_VAL val_longident COLON liquid_type           
      { (String.concat "." (Longident.flatten $2), $4) }


/* Refinement specifications */

liquid_recref_decl:
    REFINEMENT                          { None }

/* Uninterpreted functions */

liquid_uninterpreted_decl:
    UNINTERPRETED LIDENT COLON core_type { ($2, $4) }

/* Type embedding directives */

liquid_type_embedding:
    EMBED prover_type FOR core_type                   { ($4, $2) }

prover_type:
    LIDENT /* unint */                                { Pprover_abs $1 }
    | LBRACKET prover_type SEMI prover_type RBRACKET  { Pprover_array ($2, $4) }
    | LPAREN prover_type_chain RPAREN                 { Pprover_fun   ($2) }

prover_type_chain:
    prover_type MINUSGREATER prover_type              { $1 :: [$3] }
    | prover_type MINUSGREATER prover_type_chain      { $1 :: $3 }

/* Axiom declaration */

liquid_axiom_decl:
    AXIOM LIDENT COLON predicate         { ($2, $4) }

/* Measure specifications */

liquid_measure_decl:
    MEASURE LIDENT EQUAL measure_constructor_list              { (("_meas_"^$2, $2), $4) }

measure_constructor_list:
    opt_bar measure_constructor opt_measure_constructor_list   { $2 :: $3 }

opt_measure_constructor_list:
    /* empty */                                                { [] }
  | BAR measure_constructor opt_measure_constructor_list       { $2 :: $3 } 

measure_constructor:
    constr_longident opt_measure_args MINUSGREATER qual_expr
    { ((String.concat "." (Longident.flatten $1)), $2, mkpredpatorexp_exp $4) }
  | measure_arg COLONCOLON measure_arg MINUSGREATER qual_expr 
    { ("::", [$1; $3], mkpredpatorexp_exp $5) }
  | constr_longident opt_measure_args MINUSGREATER qualifier_pattern
    { (String.concat "." (Longident.flatten $1), $2, mkpredpatorexp_pred $4) }
  | measure_arg COLONCOLON measure_arg MINUSGREATER qualifier_pattern
    { ("::", [$1; $3], mkpredpatorexp_pred $5) }

opt_measure_args:
    /* empty */                                                { [] }
  | measure_arg                                                { [$1] }
  | LPAREN measure_arg_comma_list RPAREN                       { $2 }

measure_arg_comma_list:
    measure_arg                                                { [$1] }
  | measure_arg COMMA measure_arg_comma_list                   { $1 :: $3 }

measure_arg:
    LIDENT                                                     { Some $1 }
  | UNDERSCORE                                                 { None }

/* Refinement specifications */

/* Liquid types */

liquid_type_list: /* this must be before liquid_type to resolve reduce/reduces */
    liquid_type1 STAR liquid_type_list
      { $1 :: $3 }
  | liquid_type1                        %prec below_STAR
      { [$1] }

liquid_type:                             
    liquid_type_list 
      { match $1 with [st] -> st | _ -> mktrue_tuple $1  }
  | LBRACE LIDENT COLON liquid_type1 STAR liquid_type_list BAR predicate RBRACE
      { mktuple ($4::$6) ($2, $8) }
  | LBRACKET LIDENT COLON liquid_type1 RBRACKET
      { inj_name $2 $4 }

liquid_type1:
    LBRACE LIDENT COLON liquid_type2 BAR predicate RBRACE 
      { rw_frame $4 ($2, $6)  }
  | liquid_type MINUSGREATER liquid_type
      { mkarrow None $1 $3 }
  | LIDENT COLON liquid_type MINUSGREATER liquid_type
      { mkarrow (Some $1) $3 $5 }
  | liquid_type2
      { $1 }

subs_opt:
        /* empty */                                     { [] }
  | LBRACKET LIDENT INFIXOP3 LIDENT RBRACKET subs_opt   { ($2, $4) :: $6 }

liquid_type2:
    QUOTE LIDENT subs_opt                                 /* tyvar */
      { mktrue_var $2 $3 }
  | liquid_recref LIDENT                                  /* recursive tyvar */
      { mktrue_recvar $2 $1 }
  | LPAREN liquid_type_comma_list RPAREN %prec below_IDENT 
      { match $2 with [stn] -> snd stn | _ -> raise Parse_error } 
  | type_longident                                       /* base_type */
      { mktrue_constr $1 [] }
  | liquid_type type_longident                           /* simple constructed */
      { mktrue_constr $2 [(None, $1)] }
  | LPAREN liquid_type_comma_list RPAREN type_longident  /* multi-param constructed */
      { mktrue_constr $4 $2 }
  | liquid_type3
      { $1 }

liquid_type3:
    LBRACKET type_longident DOT liquid_constr_list RBRACKET
      { mktrue_sum $2 None $4 }
  | liquid_recref LBRACKET LIDENT COLON type_longident DOT liquid_constr_list RBRACKET
      { mktrue_sum $5 (Some ($3, $1)) $7 }
  | liquid_record
      { mktrue_record $1 }

liquid_constr_list:
    liquid_constr BARBAR liquid_constr_list
      { $1 :: $3 }
  | liquid_constr                       %prec below_BARBAR
      { [$1] }

liquid_constr:
    liquid_param_list
      { $1 }

liquid_param_list:
    /* empty */
      { [] }
  | liquid_param COMMA liquid_param_list
      { $1 :: $3 }
  | liquid_param
      { [$1] }

liquid_param:
    LIDENT COLON liquid_type
      { ($1, $3) }
  | LBRACE LIDENT COLON liquid_type BAR predicate RBRACE
      { ($2, rw_frame $4 ($2, $6)) }

liquid_type_comma_list:
    liquid_type
      { [(None, $1)] }
  | liquid_type COMMA liquid_type_comma_list
      { (None, $1) :: $3 }
  | LIDENT COLON liquid_type COMMA liquid_type_comma_list
      { (Some $1, $3) :: $5 }
 
liquid_record:
    LBRACE liquid_field_list RBRACE 
      { $2 }

liquid_field:
    MUTABLE LIDENT COLON liquid_type
      { ($4, $2, Mutable) }
  | LIDENT COLON liquid_type
      { ($3, $1, Immutable) }

liquid_field_list:
    liquid_field SEMI liquid_field_list
      { $1 :: $3 }
  | liquid_field
      { [$1] }

liquid_recref:
    LBRACKET liquid_constr_ref_list RBRACKET
      { $2 }

liquid_constr_ref_list:
    /* empty */
      { [] }
  | liquid_constr_ref COMMA liquid_constr_ref_list
      { $1 :: $3 }
  | liquid_constr_ref
      { [$1] }

liquid_constr_ref:
    LBRACKET liquid_elem_ref_list RBRACKET
      { $2 }

liquid_elem_ref_list:
    /* empty */
      { [] }
  | liquid_elem_ref COMMA liquid_elem_ref_list
      { $1 :: $3 }
  | liquid_elem_ref
      { [$1] }

liquid_elem_ref:
  LIDENT COLON predicate
      { ($1, $3) }

/* Predicates */

predicate:
    qualifier_pattern                       { $1 } 

/* Constants */

constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | INT32                                       { Const_int32 $1 }
  | INT64                                       { Const_int64 $1 }
  | NATIVEINT                                   { Const_nativeint $1 }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(- $2) }
  | MINUS FLOAT                                 { Const_float("-" ^ $2) }
  | MINUS INT32                                 { Const_int32(Int32.neg $2) }
  | MINUS INT64                                 { Const_int64(Int64.neg $2) }
  | MINUS NATIVEINT                             { Const_nativeint(Nativeint.neg $2) }
;
/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
val_ident_colon:
    LIDENT COLON                                { $1 }
  | LPAREN operator RPAREN COLON                { $2 }
  | LABEL                                       { $1 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { Lapply($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;

%%
