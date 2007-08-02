type binop =
    Plus
  | Minus
  | Times


type binrel =
    Eq
  | Ne
  | Lt
  | Le 

type pexpr =   
    PInt of int 
  | Var of string
  | Pvar of string * int
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr 


type predicate =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Not of predicate
  | And of predicate * predicate 
  | Or of predicate * predicate 


type parameterized_pred = PredOver of (string * predicate)

val big_and: predicate list -> predicate
val big_or: predicate list -> predicate
val equals: (pexpr * pexpr) -> predicate

val fresh_pexprvar: unit -> pexpr

val predicate_subst: pexpr -> string -> predicate -> predicate
val predicate_vars: predicate -> string list

val pprint_pexpr: pexpr -> string
val pprint_predicate: predicate -> string
