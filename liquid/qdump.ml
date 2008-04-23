open Config
open Format
open Parsetree
open Asttypes

module P = Predicate
module C = Common

module QS = Set.Make(struct
                       type t = string * string * P.t
                       let compare = compare
                     end)

let patf = ref ""
            
let expand_quals env qstrs prgids =
  let expand_squal (name, pat) =
    Qualdecl.transl_pattern_valu env prgids name pat
  in
  C.flap (expand_squal) qstrs 

let dump_qset qs =
  QS.iter (fun (nm, v, q) -> eprintf "@[squalif@ %s(%s)@ :@ %a@.@]" nm v P.pprint q) qs

let dump_default_qualifiers source =
  let _ = pp_set_margin err_formatter 1230912 in
  let _ = C.verbose_level := C.ol_dquals in
  let (str, env, fenv) = source in
  let prgids = Qualgen.visit_sstr str in
  let dqstrs = Pparse.file std_formatter !patf Parse.qualifier_patterns ast_impl_magic_number in
  let dqstrs = expand_quals env dqstrs prgids in
  let qs = List.fold_left (fun qs q -> QS.add q qs) QS.empty dqstrs in
    dump_qset qs; pp_print_flush err_formatter ()
