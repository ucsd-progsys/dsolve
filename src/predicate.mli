open Expr
open Flowgraph


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


type parameterized_pred = PredOver of (string * predicate)
type named_pred = string * parameterized_pred

val big_and: predicate list -> predicate

val predicate_subst: expression -> string -> predicate -> predicate

val expr_predicate: expr -> predicate

val value_var: expr -> expression
val branch_active: expr -> predicate
val vertex_value_var: FlowGraph.V.t -> expression
val vertex_branch_active: FlowGraph.V.t -> predicate
val pprint_predicate: predicate -> string
