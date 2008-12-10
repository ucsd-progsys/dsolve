open Typedtree
open Parsetree

let instantiate_qualifier env (name, pat) =
  let (valu, _, pred) = pat.pqual_pat_desc in
  let pred = Qualdecl.transl_patpred env (valu, Common.qual_test_var) pred in
   (Common.qual_test_var, pred)
