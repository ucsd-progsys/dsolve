open Type
open Predicate


let qplus =
  ("PLUS", PredOver("z", equals(Var "z", Binop(Var "x", Plus, Var "y"))))
let quals = [qplus]


let tplus =
  ("+", Arrow("x", Int [], Arrow("y", Int[], Int [qplus])))
let types = [tplus]
