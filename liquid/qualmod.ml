open Typedtree
open Parsetree

let qualpat_map_predpat f q = 
  let (s, t, p) = q.pqual_pat_desc in
  {pqual_pat_desc = (s, t, f p); pqual_pat_loc = q.pqual_pat_loc}
