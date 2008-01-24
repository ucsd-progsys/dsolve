open Format

type binop =
    Plus
  | Minus
  | Times
	| Div

type binrel =
    Eq
  | Ne
  | Gt
	| Ge
  | Lt
  | Le 

type pexpr =   
    PInt of int 
  | Var of Path.t
  | FunApp of string * pexpr
  | Binop of pexpr * binop * pexpr 
  | Field of string * pexpr

type t =  
    True
  | Atom of pexpr * binrel * pexpr 
  | Iff of pexpr * t
  | Not of t
  | And of t * t 
  | Or of t * t 

val pprint_rel: binrel -> string
val pprint: formatter -> t -> unit
val pprint_pexpr: formatter -> pexpr -> unit

val big_and: t list -> t
val big_or: t list -> t
val equals: (pexpr * pexpr) -> t
val implies: (t * t) -> t
val expand_iff: t -> t

val (==.): pexpr -> pexpr -> t
val (!=.): pexpr -> pexpr -> t
val (<=.): pexpr -> pexpr -> t
val (<.): pexpr -> pexpr -> t
val (>=.): pexpr -> pexpr -> t
val (>.): pexpr -> pexpr -> t
val (&&.): t -> t -> t
val (||.): t -> t -> t
val (!.): t -> t
val (=>.): t -> t -> t
val (<=>.): pexpr -> t -> t

val tuple_nth: pexpr -> int -> pexpr

val subst: pexpr -> Path.t -> t -> t
val apply_substs: (Path.t * pexpr) list -> t -> t
val vars: t -> Path.t list
(* pmr: change to plain old instantiate *)
val instantiate_named_vars: (string * Path.t) list -> t -> t
val transl_predicate: Parsetree.predicate_declaration -> t
