open OUnit
open Predicate
open Constraint


let empty_solution = Solution.create QualifierSet.empty


let simple_guard_succeed () =
  let greater_than_x = FInt([("y", PInt 4)], [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubType([], equals(Var "x", PInt 3), FInt([], []), greater_than_x) in
    assert_bool "y = 4 <= x = 3  ='(" (constraint_sat empty_solution constr)


let simple_guard_fail () =
  let greater_than_x = FInt([("y", PInt 2)], [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubType([], equals(Var "x", PInt 3), FInt([], []), greater_than_x) in
    assert_bool "y = 2 > x = 3  ='(" (not (constraint_sat empty_solution constr))


let simple_subtype_succeed () =
  let nneg = FInt([], [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))]) in
  let pos = FInt([], [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubType([], True, pos, nneg) in
    assert_bool "POS not a subtype of NNEG in empty context" (constraint_sat empty_solution constr)


let simple_subtype_fail () =
  let nneg = FInt([], [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))]) in
  let pos = FInt([], [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubType([], True, nneg, pos) in
    assert_bool "POS is a subtype of NNEG in empty context" (not (constraint_sat empty_solution constr))


let subtype_from_environment_succeed () =
  let env = [("x", FInt([], [("NNEG", PredOver("a", Atom(PInt 0, Lt, Var "a")))]))] in
  let eqx = FInt([], [("EQX", PredOver("a", Atom(Var "a", Eq, Var "x")))]) in
  let pos = FInt([], [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubType(env, True, eqx, pos) in
    assert_bool "EQX not a subtype of POS when x -> {a | 0 < a}" (constraint_sat empty_solution constr)


let subtype_from_environment_fail () =
  let env = [("x", FInt([], [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))]))] in
  let eqx = FInt([], [("EQX", PredOver("a", Atom(Var "a", Eq, Var "x")))]) in
  let pos = FInt([], [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubType(env, True, eqx, pos) in
    assert_bool "EQX a subtype of POS when x -> {a | 0 <= a}" (not (constraint_sat empty_solution constr))


let make_solution bindings =
  List.fold_right (fun (k, v) s -> Solution.add k v s) bindings empty_solution


let make_qualifierset quals =
  List.fold_right QualifierSet.add quals QualifierSet.empty


let simple_subtype_from_solution_succeed () =
  let nneg = make_qualifierset [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))] in
  let pos = make_qualifierset [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))] in
  let solution = make_solution [("k1", pos); ("k2", nneg)] in
  let constr = SubType([], True, FVar([], "k1"), FVar([], "k2")) in
    assert_bool "k1 = POS not a subtype of k2 = NNEG with partial solution"
      (constraint_sat solution constr)


let simple_subtype_from_solution_fail () =
  let nneg = make_qualifierset [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))] in
  let pos = make_qualifierset [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))] in
  let solution = make_solution [("k1", pos); ("k2", nneg)] in
  let constr = SubType([], True, FVar([], "k2"), FVar([], "k1")) in
    assert_bool "k2 = NNEG is a subtype of k1 = POS with partial solution"
      (not (constraint_sat solution constr))


let suite = "Test Constraint Solving" >:::
  ["Test sat: simple guard expression (successful)" >:: simple_guard_succeed;
   "Test sat: simple guard expression (unsuccessful)" >:: simple_guard_fail;
   "Test sat: simple subtyping relation (successful)" >:: simple_subtype_succeed;
   "Test sat: simple subtyping relation (unsuccessful)" >:: simple_subtype_fail;
   "Test sat: subtype depending on environment (successful)" >:: subtype_from_environment_succeed;
   "Test sat: subtype depending on environment (unsuccessful)" >:: subtype_from_environment_fail;
   "Test sat: subtype depending on partial solution (successful)" >:: simple_subtype_from_solution_succeed;
   "Test sat: subtype depending on partial solution (unsuccessful)" >:: simple_subtype_from_solution_fail;
  ]
