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

type patpexpr =
    PPInt of int list
  | PVar of Path.t list
  | PFunApp of Longident.t * patpexpr list 
  | PBinop of patpexpr * binop list * patpexpr
  | PField of string * patpexpr
  | PProj of int * patpexpr

type tpat =
    PTrue
  | PAtom of patpexpr * binrel list * patpexpr
  | PIff of patpexpr * tpat
  | PNot of tpat
  | PAnd of tpat * tpat
  | POr of tpat * tpat

type pexpr =   
    PInt of int 
  | Var of Path.t
  | FunApp of string * pexpr list 
  | Binop of pexpr * binop * pexpr 
  | Field of string * pexpr     (* INVARIANT: disjoint fields in same module *)
  | Proj of int * pexpr

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
val int_true: pexpr
val int_false: pexpr
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
val (+-): pexpr -> pexpr -> pexpr
val ( *-): pexpr -> pexpr -> pexpr
val ( /-): pexpr -> pexpr -> pexpr
val (--): pexpr -> pexpr -> pexpr

val subst: pexpr -> Path.t -> t -> t
val apply_substs: (Path.t * pexpr) list -> t -> t
val vars: t -> Path.t list
(* pmr: change to plain old instantiate *)
val instantiate_named_vars: (string * Path.t) list -> t -> t
val transl_op: Asttypes.predexp_op -> binop                                                             
val transl_rel: Asttypes.pred_rel -> binrel
