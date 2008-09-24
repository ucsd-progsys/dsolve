(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

module C = Common
module P = Predicate
module QA = QpAst
module QDP = QpDag.Predicate
module QAP = QpAst.Predicate
module Cl = Clflags

(**************************************************************************)
(**************** Statistics Counters Etc. ********************************)
(**************************************************************************)

let nb_push = ref 0
let nb_valid = ref 0
let nb_queries = ref 0

(**************************************************************************)
(**************** Converting to QProver Syntax ****************************)
(**************************************************************************)

let div_sym = "DIV"
let mul_sym = "MUL"
let fld_sym = "SELECT_"

let rec convert p : QpAst.predicate = failwith "TBD: convert to QpAst.predicate"
   
let mk_const i = QA.Constant (QA.Constant.Int i)
let is_const = function P.PInt i -> Some i | _ -> None 

let rec convertExp = function
  | P.PInt i -> mk_const i 
  | P.Var s -> QA.Variable (Path.unique_name s) 
  | P.FunApp (f, e) -> QA.Application (f, List.map convertExp e)
  | P.Field (f, e) -> QA.Application (fld_sym^(Ident.unique_name f), [convertExp e])
  | P.Binop (e1,P.Plus,e2) -> QA.Sum [convertExp e1; convertExp e2] 
  | P.Binop (e1,P.Minus,e2) -> QA.Sum [convertExp e1; QA.Coeff (QA.Constant.Int (-1), convertExp e2)] 
  | P.Binop (e1,P.Div,e2) -> QA.Application (mul_sym, [convertExp e1; convertExp e2])
  | P.Binop (e1,P.Times,e2) ->
      (match (is_const e1, is_const e2) with
      | (Some i1, Some i2) -> mk_const (i1 * i2)
      | (Some i1, None) -> QA.Coeff ((QA.Constant.Int i1), convertExp e2)
      | (None, Some i2) -> QA.Coeff ((QA.Constant.Int i2), convertExp e1)
      | (None, None) -> QA.Application (mul_sym, [convertExp e1; convertExp e2]))
  | P.Ite (t, e1, e2) -> QA.Ite (convertPred t, convertExp e1, convertExp e2)

and convertPred = function 
  | P.True -> QA.True 
  | P.Not p -> QA.Not (convertPred p)  
  | P.And (p1,p2) -> QA.And [convertPred p1; convertPred p2] 
  | P.Or (p1,p2) -> QA.Or [convertPred p1; convertPred p2]
  | P.Iff (p,q) -> (*convertPred (P.expand_iff p)*) assert false
  | P.Atom (e1,br,e2) ->
      let (e1',e2') = (convertExp e1, convertExp e2) in 
      (match br with 
       | P.Eq -> QA.Equality (e1',e2') 
       | P.Ne -> QA.Not (QA.Equality (e1',e2'))
       | P.Le -> QA.Leq (e1',e2') 
       | P.Ge -> QA.Leq (e2',e1')
       | P.Lt -> QA.Leq (QA.Sum [e1'; mk_const 1], e2')
       | P.Gt -> QA.Leq (QA.Sum [e2'; mk_const 1], e1'))
  | P.Forall _ | P.Exists _ | P.Boolexp _ -> assert false 

let convertPredDag p = 
  QpDag.pred_dag_of_tree (convertPred p)

(**************************************************************************)
(********************************* API ************************************)
(**************************************************************************)


(* API *)
let print_stats () = 
  QProver.print_stats ();
  Printf.printf "QProver pushes = %d, queries = %d, valid = %d \n" 
  !nb_push !nb_queries !nb_valid

(* API *)
let implies p =
  let _ = incr nb_push in
  let p' = convertPred p in
  let _ = if !Cl.qpdump then C.write_to_file "liquid.qp" (QAP.toString p') in
  let qpc = QProver.check_imp (QpDag.pred_dag_of_tree p') in
  fun q -> 
    let q' = convertPred q in 
    let _ = if !Cl.qpdump then C.append_to_file "liquid.qp" ("\n ; \n"^(QAP.toString q')^")") in
    let rv = qpc (QpDag.pred_dag_of_tree q') in
    let _ = incr nb_queries; (if rv then incr nb_valid) in
    rv
