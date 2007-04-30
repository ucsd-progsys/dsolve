(*
 * Copyright © 1990-2007 The Regents of the University of California. All rights reserved. 
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

(* This file is part of the SIMPLE Project.*)

open Expr
open Predicate


module type PROVER = 
  sig
    (* push p: tell prover to assume fact p *)
    val push : Predicate.predicate -> unit 
    
    (* pop () : tell prover to un-assume last assumed fact *)
    val pop : unit -> unit 
    
    (* reset (): tell prover to remove all assumed facts *)
    val reset : unit -> unit 
    
    (* valid p : do the currently assumed facts imply p ? *)
    val valid : Predicate.predicate -> bool

    (* implies p q = true iff predicate p (provably) implies predicate q *)
    val implies : Predicate.predicate -> Predicate.predicate -> bool  
    
  end


module Y = Oyices

module YicesProver  = 
  struct
    
    type yices_instance = { 
      c : Y.yices_context;
      t : Y.yices_type;
      d : (string,Y.yices_var_decl) Hashtbl.t;
      mutable ds : string list ;
      mutable count : int;
      mutable i : int;
    }

    let barrier = "0" 

    let yicesVar me s =
      let decl = 
        Misc.do_memo me.d
        (fun () -> 
          let rv = Y.yices_mk_var_decl me.c s me.t in
            me.ds <- s::me.ds;rv) () s in
      Y.yices_mk_var_from_decl me.c decl
    
    let rec yicesExp me e =
      match e with 
        Int i -> Y.yices_mk_num me.c i 
      | Var s -> yicesVar me s 
      | Pvar (s,i) -> yicesVar me (Printf.sprintf "%sprime%d" s i) 
      | Binop (e1,op,e2) ->
          let es' = Array.map (yicesExp me) [|e1;e2|] in
          (match op with 
             Plus  -> Y.yices_mk_sum me.c es'
           | Minus -> Y.yices_mk_sub me.c es'
           | Times -> Y.yices_mk_mul me.c es')

    let rec yicesPred me p = 
      match p with 
        True -> Y.yices_mk_true me.c
      | Not p' -> Y.yices_mk_not me.c (yicesPred me p')
      | And (p1,p2) -> Y.yices_mk_and me.c (Array.map (yicesPred me) [|p1;p2|])
      | Or (p1,p2) -> Y.yices_mk_or me.c (Array.map (yicesPred me) [|p1;p2|])
      | Atom (e1,br,e2) ->
          let e1' = yicesExp me e1 in
          let e2' = yicesExp me e2 in
          (match br with 
             Eq -> Y.yices_mk_eq me.c e1' e2' 
           | Ne -> Y.yices_mk_diseq me.c e1' e2' 
           | Lt -> Y.yices_mk_lt me.c e1' e2'
           | Le -> Y.yices_mk_le me.c e1' e2')

    let me = 
      let c = Y.yices_mk_context () in
      let t = Y.yices_mk_type c "object" in
      let d = Hashtbl.create 37 in
        { c = c; t = t; d = d; ds = []; count = 0; i = 0 }

    let push p =
      me.count <- me.count + 1;
      if Y.yices_inconsistent me.c = 1 then
	me.i <- me.i + 1
      else
	begin
	  me.ds <- barrier :: me.ds;
	  Y.yices_push me.c;
	  Y.yices_assert me.c (yicesPred me p)
	end
      
    let rec vpop (cs,s) =
      match s with [] -> (cs,s)
      | h::t when h = barrier -> (cs,t)
      | h::t -> vpop (h::cs,t)

    let pop () =
      me.count <- me.count - 1;
      if me.i > 0 then
	me.i <- me.i - 1
      else
	begin
	  let (cs,ds') = vpop ([],me.ds) in
	    me.ds <- ds';
	    List.iter (Hashtbl.remove me.d) cs;
	    Y.yices_pop me.c
	end

    let reset () =
      Misc.repeat_fn pop me.count

    let unsat () =
      Y.yices_check me.c = -1

    let valid p =
      let _ = push (Not p) in
      let rv = unsat () in
      let _ = pop () in
      rv

    let implies p q = 
      let _ = push p in
      let rv = valid q in
      let _ = pop () in
      rv
  end

module Prover = YicesProver
