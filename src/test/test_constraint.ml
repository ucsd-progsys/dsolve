open OUnit
open Predicate
open Constraint


let simple_guard_succeed () =
  let greater_than_x = FInt([("y", PInt 4)], [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubType([], equals(Var "x", PInt 3), FInt([], []), greater_than_x) in
    assert_bool "y = 4 <= x = 3  ='(" (constraint_sat Solution.empty constr)


let simple_guard_fail () =
  let greater_than_x = FInt([("y", PInt 2)], [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubType([], equals(Var "x", PInt 3), FInt([], []), greater_than_x) in
    assert_bool "y = 2 > x = 3  ='(" (not (constraint_sat Solution.empty constr))


let suite = "Test Constraint Solving" >:::
  ["Test simple guard expression (successful)" >:: simple_guard_succeed;
   "Test simple guard expression (unsuccessful)" >:: simple_guard_fail;
  ]
