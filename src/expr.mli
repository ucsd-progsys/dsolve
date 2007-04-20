open Env


type qual =
    Top
  | Bottom
  | Qual of string
  | QualVar of string
  | QualMeet of qual * qual
  | QualJoin of qual * qual

type typ =
    Arrow of qual * typ * typ
  | Int of qual
  | Bool of qual
  | TyVar of qual * string
  | Nil
  | ForallQual of string * qual * typ
  | ForallTyp of string * typ


type binop =
    Plus
  | Minus
  | Times 

type binrel =
    Eq
  | Ne
  | Lt
  | Le 

type expr_id = int

type expr =
    Num of int * expr_id
  | TrueExp of expr_id
  | FalseExp of expr_id
  | ExpVar of string * expr_id
  | BinOp of binop * expr * expr * expr_id
  | BinRel of binrel * expr * expr * expr_id
  | If of expr * expr * expr * expr_id
  | Annot of qual * expr * expr_id
  | Let of string * typ option * expr * expr * expr_id
  | Abs of string * typ option * expr * expr_id
  | App of expr * expr * expr_id
  | TyAbs of string * expr * expr_id
  | TyApp of expr * typ * expr_id
  | QualAbs of string * qual * expr * expr_id
  | QualApp of expr * qual * expr_id

type value =
    NumVal of int
  | TrueVal
  | FalseVal
  | Closure of string * expr * value env


exception BogusEvalError


val eval : expr -> value
val expr_get_id : expr -> expr_id

val pprint_binop: ('a -> string) -> 'a -> binop -> 'a -> string
val pprint_binrel: ('a -> string) -> 'a -> binrel -> 'a -> string
val pprint_expr: expr -> string
val pprint_value : value -> string
