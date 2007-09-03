open OUnit
open Predicate
open Constraint
open Frame


let empty_solution = Solution.create QualifierSet.empty


let simple_guard_succeed () =
  let greater_than_x = ([("y", PInt 4)], RQuals [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubRef(Env.empty, equals(Var "x", PInt 3), ([], RQuals []), greater_than_x) in
    assert_bool "y = 4 <= x = 3  ='(" (constraint_sat empty_solution constr)


let simple_guard_fail () =
  let greater_than_x = ([("y", PInt 2)], RQuals [("GREATERX", PredOver("a", Atom(Var "x", Lt, Var "y")))]) in
  let constr = SubRef(Env.empty, equals(Var "x", PInt 3), ([], RQuals []), greater_than_x) in
    assert_bool "y = 2 > x = 3  ='(" (not (constraint_sat empty_solution constr))


let simple_subtype_succeed () =
  let nneg = ([], RQuals [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))]) in
  let pos = ([], RQuals [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubRef(Env.empty, True, pos, nneg) in
    assert_bool "POS not a subtype of NNEG in empty context" (constraint_sat empty_solution constr)


let simple_subtype_fail () =
  let nneg = ([], RQuals [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))]) in
  let pos = ([], RQuals [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubRef(Env.empty, True, nneg, pos) in
    assert_bool "POS is a subtype of NNEG in empty context" (not (constraint_sat empty_solution constr))


let subtype_from_environment_succeed () =
  let env = Env.add "x" (FInt([], RQuals [("NNEG", PredOver("a", Atom(PInt 0, Lt, Var "a")))])) Env.empty in
  let eqx = ([], RQuals [("EQX", PredOver("a", Atom(Var "a", Eq, Var "x")))]) in
  let pos = ([], RQuals [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubRef(env, True, eqx, pos) in
    assert_bool "EQX not a subtype of POS when x -> {a | 0 < a}" (constraint_sat empty_solution constr)


let subtype_from_environment_fail () =
  let env = Env.add "x" (FInt([], RQuals [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))])) Env.empty in
  let eqx = ([], RQuals [("EQX", PredOver("a", Atom(Var "a", Eq, Var "x")))]) in
  let pos = ([], RQuals [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))]) in
  let constr = SubRef(env, True, eqx, pos) in
    assert_bool "EQX a subtype of POS when x -> {a | 0 <= a}" (not (constraint_sat empty_solution constr))


let make_solution bindings =
  List.fold_right (fun (k, v) s -> Solution.add k v s) bindings empty_solution


let simple_subtype_from_solution_succeed () =
  let nneg = QualifierSet.from_list [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))] in
  let pos = QualifierSet.from_list [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))] in
  let solution = make_solution [("k1", pos); ("k2", nneg)] in
  let constr = SubRef(Env.empty, True, ([], RVar "k1"), ([], RVar "k2")) in
    assert_bool "k1 = POS not a subtype of k2 = NNEG with partial solution"
      (constraint_sat solution constr)


let simple_subtype_from_solution_fail () =
  let nneg = QualifierSet.from_list [("NNEG", PredOver("a", Atom(PInt 0, Le, Var "a")))] in
  let pos = QualifierSet.from_list [("POS", PredOver("b", Atom(PInt 0, Lt, Var "b")))] in
  let solution = make_solution [("k1", pos); ("k2", nneg)] in
  let constr = SubRef(Env.empty, True, ([], RVar "k2"), ([], RVar "k1")) in
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
