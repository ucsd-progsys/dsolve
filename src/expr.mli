open Env


type qualliteral =
    Top
  | Bottom
  | Qual of string

type qual =
    QualLiteral of qualliteral
  | QualVar of string
  | QualMeet of qual * qual
  | QualJoin of qual * qual

type monotyp =
    Arrow of qual * monotyp * monotyp
  | Int of qual
  | Bool of qual
  | TyVar of qual * string
  | Nil

type qualschema =
    ForallQual of string * qualliteral * qualschema
  | MonoTyp of monotyp

type typschema =
    ForallTyp of string * typschema
  | QSchema of qualschema


type expr =
    Num of int
  | True
  | False
  | Var of string
  | If of expr * expr * expr
  | Annot of qual * expr
  | Let of string * typschema * expr * expr
  | Abs of string * monotyp * expr
  | App of expr * expr
  | TyAbs of string * expr
  | TyApp of expr * monotyp
  | QualAbs of string * qualliteral * expr
  | QualApp of expr * qualliteral

type value =
    NumVal of int
  | TrueVal
  | FalseVal
  | Closure of string * expr * value env


exception BogusEvalError


val eval: expr -> value
