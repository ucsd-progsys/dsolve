open Expr


type expression =   
    Int of int 
  | Var of string
  | Pvar of string * int
  | Binop of expression * binop * expression 


type predicate =  
    True
  | Atom of expression * binrel * expression 
  | Not of predicate
  | And of predicate * predicate 
  | Or of predicate * predicate 


type parameterized_pred = PredOver of string * string * predicate

val predicate_subst: expression -> string -> predicate -> predicate

val expr_predicate: expr -> predicate

val value_var: expr -> expression
val branch_active: expr -> predicate
val pprint_predicate: predicate -> string
