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


type expr =
    Num of int
  | True
  | False
  | Var of string
  | If of expr * expr * expr
  | Annot of qual * expr
  | Let of string * typ option * expr * expr
  | Abs of string * typ option * expr
  | App of expr * expr
  | TyAbs of string * expr
  | TyApp of expr * typ
  | QualAbs of string * qual * expr
  | QualApp of expr * qual

type value =
    NumVal of int
  | TrueVal
  | FalseVal
  | Closure of string * expr * value env


exception BogusEvalError


val eval: expr -> value
