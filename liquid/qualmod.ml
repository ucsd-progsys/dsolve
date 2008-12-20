open Typedtree
open Parsetree

let qualpat_map_predpat f q = 
  let (s, t, p) = q.pqual_pat_desc in
  {pqual_pat_desc = (s, t, f p); pqual_pat_loc = q.pqual_pat_loc}

(*let instantiate_qualifier env consts (name, pat) =
  let (valu, types, pred) = pat.pqual_pat_desc in
  let pred = Qualdecl.transl_patpred env (valu, Common.qual_test_var) types consts pred in
   (Common.qual_test_var, pred)*)

