module D = Dconstraint
module DP = Path

module F = Fconstraint

let f_of_drefcons = function
  SubRef (renvt, gd, r1, r2, id) -> 
  WFRef  (envt, r, id) -> 
