open Format

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
  | Var of Ident.t
  | Pvar of Ident.t * int
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr 

type t =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Not of t
  | And of t * t 
  | Or of t * t 

val pprint: formatter -> t -> unit
val pprint_pexpr: formatter -> pexpr -> unit

val big_and: t list -> t
val big_or: t list -> t
val equals: (pexpr * pexpr) -> t

val subst: pexpr -> Ident.t -> t -> t
val vars: t -> Ident.t list
(* pmr: change to plain old instantiate *)
val instantiate_named_vars: (string * Ident.t) list -> t -> t
val transl_predicate: Parsetree.predicate_declaration -> t

