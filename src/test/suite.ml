open OUnit

let suite = "MicroML Test Suite" >:::
  [Test_constraint.suite]

let _ =
  run_test_tt_main suite
  
