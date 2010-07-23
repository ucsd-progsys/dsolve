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
  | Lit _        -> true
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

let test_nnf f =
  let n = to_nnf f in
    check_nnf n

let rec is_disjunction = function
  | Or (f1, f2) -> is_disjunction f1 && is_disjunction f2
  | Lit _       -> true
  | And _       -> false
  | Not _       -> false

let rec is_cnf = function
  | And (f1, f2) -> is_cnf f1 && is_cnf f2
  | Or (f1, f2)  -> is_disjunction f1 && is_disjunction f2
  | Lit _        -> true
  | Not _        -> false

let rec dist_or f1 f2 =
  match f1, f2 with
    | Or (f1, f2), Or (f3, f4)   -> Or (Or (f1, f2), Or (f3, f4))
    | And (f1, f2), f3           -> And (dist_or f1 f3, dist_or f2 f3)
    | f1, And (f2, f3)           -> And (dist_or f1 f2, dist_or f1 f3)
    | Lit l1, Lit l2             -> Or (Lit l1, Lit l2)
    | Or (f1, f2), Lit l         -> Or (Or (f1, f2), Lit l)
    | Lit l, Or (f1, f2)         -> Or (Or (f1, f2), Lit l)
    | Not _, _                   -> assert (0 = 1); assert false
    | _, Not _                   -> assert (0 = 1); assert false

let rec to_cnf = function
  | Or (f1, f2)  -> dist_or (to_cnf f1) (to_cnf f2)
  | And (f1, f2) -> And (to_cnf f1, to_cnf f2)
  | Lit l        -> Lit l
  | Not _        -> assert (0 = 1); assert false

let rec check_disjunction = function
  | Lit _        -> ()
  | Or (f1, f2)  -> check_disjunction f1; check_disjunction f2
  | And _        -> assert (0 = 1)
  | Not _        -> assert (0 = 1)

let rec check_cnf = function
  | Lit _        -> ()
  | And (f1, f2) -> check_cnf f1; check_cnf f2
  | Or (f1, f2)  -> check_disjunction f1; check_disjunction f2
  | Not _        -> assert (0 = 1)

let test_cnf f =
  let n = to_nnf f in
  let c = to_cnf n in
    check_cnf c

