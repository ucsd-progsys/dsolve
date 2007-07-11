type binop =
    Plus
  | Minus
  | Times


type binrel =
    Eq
  | Ne
  | Lt
  | Le 

type expression =   
    PInt of int 
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

val big_and: predicate list -> predicate
val big_or: predicate list -> predicate
val equals: (expression * expression) -> predicate

val fresh_expressionvar: unit -> expression

val predicate_subst: expression -> string -> predicate -> predicate
val predicate_vars: predicate -> string list

val pprint_expression: expression -> string
val pprint_predicate: predicate -> string
