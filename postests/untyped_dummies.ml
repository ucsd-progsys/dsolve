type expr = 
  | Lam of string * expr
  | Var of string

let e2 = Lam ("x", Var "x")
