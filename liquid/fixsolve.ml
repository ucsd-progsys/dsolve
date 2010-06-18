(*
module C = Common

module FCo = FixConstraint
module A   = Ast
module Asm = A.Symbol.SMap

module P = Predicate
module F = Frame
module Co = Constraint
module Le = Liqenv

module Cg = Consglue

let solver fname max_env cs soln =
  (* translate to fixpoint *)
  let fmax_env = Cg.inject_tag (Cg.f_of_denvt max_env) in
  let fsort_max_env = Asm.map Cg.fsort_of_reft fmax_env in
  let fcs = Cg.f_of_dsubcons fmax_env cs in
  let fwfs = Cg.f_of_dwfcons cs in
  let soln = Cg.f_of_dsoln soln in
  let _ = Format.printf "@[InitSoln:@\n%a@]" FCo.print_soln soln in
  (* solve with fixpoint *)
  let (solver, _) = Solve.create [] fsort_max_env [] 0 [] fcs fwfs [] in
  let bname       = Miscutil.chop_extension_if_any fname in
  let _           = Solve.save (bname ^ ".ml.in.fq") solver soln in
  let (soln, _)   = Solve.solve solver soln in
  let _           = Format.printf "@[FinSoln:@\n%a@]" FCo.print_soln soln in
  let _           = Solve.save (bname ^ ".ml.out.fq") solver soln in
  Cg.d_of_fsoln soln

  *)
