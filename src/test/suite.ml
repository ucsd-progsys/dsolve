open OUnit

let suite = "MicroML Test Suite" >:::
  [Test_flowgraph.suite]

let _ =
  run_test_tt_main suite
  
