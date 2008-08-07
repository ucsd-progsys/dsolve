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

(* This file is part of the Dsolve Project.*)

(* open Predicate *)

module P = Predicate
module C = Common

module type PROVER = 
  sig
    (** 
     implies p q = true iff predicate p (provably) implies predicate q 
     *)
    val implies : P.t -> P.t -> bool
    
    (** 
     finish () undoes any pushing 
     *)
    val finish : unit -> unit
    
    val print_stats : Format.formatter -> unit -> unit
  end

module Z3Prover : PROVER = 
  struct
    type z3_instance = { 
      c                 : Z3.context;
      tint              : Z3.ast_type;
      vart              : (string,Z3.ast) Hashtbl.t;
      funt              : (string,Z3.const_decl_ast) Hashtbl.t;
      mutable vars      : string list ;
      mutable count     : int;
      mutable i         : int
    }

    (* stats *)
    let nb_z3_push = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop = ref 0
    let nb_implies_api = ref 0

    let barrier = "0" 
    
    let z3Var me s ty =
      Misc.do_memo me.vart
      (fun () -> 
        let sym = Z3.mk_string_symbol me.c s in
        let rv = Z3.mk_const me.c sym ty in
        me.vars <- s::me.vars; rv) 
      () s 

    let z3Fun me s k = 
      Misc.do_memo me.funt
      (fun () ->
        let sym = Z3.mk_string_symbol me.c s in
        let rv  = Z3.mk_func_decl me.c sym (Array.make k me.tint) me.tint in
        rv) 
      () s

    let z3App me s zes =
      let k   = List.length zes in
      let zes = Array.of_list zs in
      Z3.mk_app me.c (z3Fun me s k) zes 

    let rec isConst = function P.PInt i -> true | _ -> false

    let rec z3Exp me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var me (Path.unique_name s) me.tint
      | P.FunApp (f,es)         -> z3App me f (List.map (z3Exp me) es)
      | P.Binop (e1,P.Plus,e2)  -> Z3.mk_add me.c (Array.map (z3Exp me) [|e1;e2|] 
      | P.Binop (e1,P.Minus,e2) -> Z3.mk_sub me.c (Array.map (z3Exp me) [|e1;e2|] 
      | P.Binop (e1,P.Times,e2) -> Z3.mk_mul me.c (Array.map (z3Exp me) [|e1;e2|] 
      | P.Binop (e1,P.Div,e2)   -> z3App me "_DIV" (List.map (z3Exp me) [e1;e2])  
      | P.Field (f, e)          -> z3App me ("SELECT_"^(Ident.unique_name f)) [z3Exp me] (** REQUIRES: disjoint intra-module field names *)
      | P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred me e1) (z3Exp me e2) (z3Exp me e3)

    and yicesPred me p = 
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (yicesPred me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (yicesPred me) [|p1;p2|])
      | P.Iff _ as iff -> z3Pred me (P.expand_iff iff)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) -> Z3.mk_eq me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Ne,e2) -> Z3.mk_distinct me.c [|z3Exp me e1; z3Exp me e2|]
      | P.Atom (e1,P.Gt,e2) -> Z3.mk_gt me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Ge,e2) -> Z3.mk_ge me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Lt,e2) -> Z3.mk_lt me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Le,e2) -> Z3.mk_le me.c (z3Exp me e1) (z3Exp me e2)
   
   
    let unsat me =
      let _ = incr nb_z3_unsat in
      let rv = (Bstats.time "Z3 unsat" Z3.check me.c) = Z3.L_FALSE in
      rv

    let push me p =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      if unsat me then me.i <- me.i + 1 else
        let _  = me.vars <- barrier :: me.vars in
        let _  = Z3.push me.c in
        Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) (z3Pred me p) 
      
    let rec vpop (cs,s) =
      match s with [] -> (cs,s)
      | h::t when h = barrier -> (cs,t)
      | h::t -> vpop (h::cs,t)

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
      if me.i > 0 then me.i <- me.i - 1 else
        let (cs,vars') = vpop ([],me.vars) in
	let _ = me.vars <- vars' in
	let _ = List.iter (Hashtbl.remove me.vart) cs in
        Z3.pop me.c 1

    let valid me p =
      if unsat me then true else 
        let _  = push me (P.Not p) in
        let rv = unsat me in
        let _  = pop me in rv
    
    let me = 
      let c = Z3.mk_context_x [|("MODEL", "false")|] in
      let _ = if !Clflags.log_queries then ignore (Z3.trace_to_file "z3.log") in 
      let tint = Z3.mk_int_type c in
      let vdect = Hashtbl.create 37 in
      let fdect = Hashtbl.create 37 in
      { c = c; tint = tint; vdect = vdect; fdect = fdect; 
        vars = []; count = 0; i = 0}


(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    let implies p =
      let _ = incr nb_implies_api in
      let _ = Bstats.time "Z3 push" (push me) p in
      fun q -> Bstats.time "Z3 valid" (valid me) q

    let finish () = 
      Bstats.time "Z3 pop" pop me; 
      assert (me.count = 0)
 
    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Yices@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_implies_api !nb_z3_push !nb_z3_pop !nb_z3_unsat

end

module Prover = Z3Prover 
