open Typedtree
open Parsetree

let type_qualifier (name, pat) =
  let (valu, _, pred) = pat.pqual_pat_desc in
  let valu = Ident.create valu in
  let pred = Qualdecl.transl_patpred_single true (Path.Pident valu) Env.empty pred in
   Tstr_qualifier (Ident.create name, (valu, pred))

let map_preds f quals =
  let map_pred = function
      Tstr_qualifier (id, (v, p)) -> Tstr_qualifier (id, f (v, p))
    | t -> t in
  List.map map_pred quals
