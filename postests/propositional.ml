(* DSOLVE -bare -dontminemlq -hide-rectypes *)

type var = int

type literal =
  | Var of var
  | Neg of var

let negate_literal = function
  | Var v -> Neg v
  | Neg v -> Var v

type formula =
  | Lit of literal
  | And of formula * formula
  | Or  of formula * formula
  | Not of formula

let rec is_nnf = function
  | Lit l        -> true
  | And (f1, f2) -> is_nnf f1 && is_nnf f2
  | Or (f1, f2)  -> is_nnf f1 && is_nnf f2
  | Not _        -> false

let negate f =
  Not f

let rec to_nnf = function
  | Not (And (f1, f2)) -> Or (to_nnf (negate f1), to_nnf (negate f2))
  | Not (Or (f1, f2))  -> And (to_nnf (negate f1), to_nnf (negate f2))
  | Not (Lit l)        -> Lit (negate_literal l)
  | Not (Not f)        -> to_nnf f
  | And (f1, f2)       -> And (to_nnf f1, to_nnf f2)
  | Or (f1, f2)        -> Or (to_nnf f1, to_nnf f2)
  | Lit l              -> Lit l

let rec check_nnf = function
  | Lit _        -> ()
  | And (f1, f2) -> check_nnf f1; check_nnf f2
  | Or (f1, f2)  -> check_nnf f1; check_nnf f2
  | Not _        -> assert (0 = 1)

let test f =
  let n = to_nnf f in
    check_nnf n
