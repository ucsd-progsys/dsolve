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

module Co  = Common
module P   = Predicate
module F   = Frame
module Le  = Liqenv
module IM  = Misc.IntMap
module C   = Constants
module Q   = Qualifier
open Format
open Wellformed
open Misc.Ops

module Sol = struct
  include Hashtbl.Make(struct
                         type t    = F.qvar
                         let hash  = Hashtbl.hash
                         let equal = (=)
                       end)

  type s = Qualifier.t list t

  let size s =
    fold (fun _ qs x -> (+) x (List.length qs)) s 0

  let dump s =
    if C.ck_olev C.ol_solve then
      let bs = fold (fun p r l -> (p, r) :: l) s [] in
      let bs = List.sort (fun (p, _) (p', _) -> compare p p') bs in
      List.iter (fun (p, r) -> C.cprintf C.ol_solve "@[k%d: %a@]@."
                p (Oprint.print_list Q.pprint Co.space) r) bs
    else ()
end


(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type fc_id = int option 
type subref_id = int 
type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of F.t Le.t * guard_t * F.t * F.t
  | WFFrame of F.t Le.t * F.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t 
  | Assert of Location.t 
  | Cstr of labeled_constraint

type refinement_constraint =
  | FixRef of FixConstraint.t
(*| FixWF  of FixConstraint.wf  *)
  | SubRef of F.refinement Le.t * guard_t * F.refinement * F.simple_refinement * (subref_id option)
  | WFRef of F.t Le.t * F.simple_refinement * (subref_id option)

(**************************************************************)
(***************** Misc. Constants / Accessors ****************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

(* Unique variable to qualify when testing sat, 
 * applicability of qualifiers...
 * this is passed into the solver *)

let qual_test_var = Co.qual_test_var(*Path.mk_ident "AA"*)
let qual_test_expr = P.Var qual_test_var

let is_simple_constraint c = match c with 
  | SubRef (_, _, r1, ([], F.Qvar _), _) ->
      List.for_all (function ([], ([], _)) -> true | _ -> false) r1
  | _ -> false

let is_simple_constraint2 = function 
  | SubRef (_, _, [([], ([], [k1]))], ([], F.Qvar k2), _) -> true
  | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let is_subframe_constraint = function
  SubFrame _ -> true | _ -> false

let is_wfframe_constraint = function
  WFFrame _ -> true | _ -> false

let solution_map s k = 
  Misc.do_catch 
    (Printf.sprintf "ERROR: solution_map couldn't find: k%d" k)
    (Sol.find s) k  

let sref_map f r =
  let (qconsts, qvars) = F.ref_to_simples r in 
  List.map f (qconsts @ qvars)

let guard_predicate g = 
  g |> List.map (fun (v,b) -> (P.(?.) (P.Var v), b))
    |> List.map (fun (p,b) -> if b then p else P.Not p)
    |> P.big_and

let refinement_preds sm qexpr r =
  F.refinement_conjuncts sm qexpr r

let environment_preds sm env = 
  List.flatten (Le.maplist (fun v r -> refinement_preds sm (P.Var v) r) env)

let sol_of_solmap (soln: Qualifier.t list IM.t) =
  Sol.create 108 >> (fun s -> IM.iter (fun k qs -> Sol.replace s k qs) soln)
