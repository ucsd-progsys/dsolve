open Typedtree
open Parsetree

let instantiate_qualifier env consts (name, pat) =
  let (valu, types, pred) = pat.pqual_pat_desc in
  let pred = Qualdecl.transl_patpred env (valu, Common.qual_test_var) types consts pred in
   (Common.qual_test_var, pred)
