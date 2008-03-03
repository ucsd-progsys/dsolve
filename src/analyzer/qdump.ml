open Config
open Format
open Parsetree
open Asttypes

module Qg = Qualgen
module Qd = Qualdecl
module P = Predicate
module C = Common

module QS = Set.Make(struct
                       type t = string * P.t
                       let compare = compare
                     end)

let patf = ref ""
            
let expand_quals env qstrs tymap idset intset =
  let expand_squal q =
    match q.pstr_desc with
      Pstr_qualifier (name, pat) ->
        Qd.transl_pattern_valu env pat tymap idset intset
      (* Pstr_prequalifier *)
      | _ -> []
  in
  C.flap (expand_squal) qstrs 

let dump_qset qs =
  let n = ref 0 in
  let nx () = incr n; !n in
    QS.iter (fun (v, q) -> eprintf "@[squalif@ Q%i(%s)@ :@ %a@.@]" (nx ()) v P.pprint q) qs

let dump_default_qualifiers source =
  let _ = pp_set_margin err_formatter 1230912 in
  let _ = C.verbose_level := C.ol_dquals in
  let (str, env, fenv) = source in
  let _ = Qg.visit_str str in
  let (dqstrs, tymap, idset, intset) = Pparse.file std_formatter !patf Parse.implementation ast_impl_magic_number in 
  let dqstrs = expand_quals env dqstrs tymap idset intset in
  let qs = List.fold_left (fun qs q -> QS.add q qs) QS.empty dqstrs in
    dump_qset qs; pp_print_flush err_formatter ()
