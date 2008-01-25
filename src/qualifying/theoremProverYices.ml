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

open Predicate


module type PROVER = 
  sig
    (* push p: tell prover to assume fact p *)
    val push : Predicate.t -> unit 
    
    (* pop () : tell prover to un-assume last assumed fact *)
    val pop : unit -> unit 
    
    (* reset (): tell prover to remove all assumed facts *)
    val reset : unit -> unit 
    
    (* valid p : do the currently assumed facts imply p ? *)
    val valid : Predicate.t -> bool

    (* implies (p, q) = true iff predicate p (provably) implies predicate q *)
    val implies : (Predicate.t * Predicate.t) -> bool
    
  end


module Y = Oyices

module YicesProver  = 
  struct
    
    type yices_instance = { 
      mutable c : Y.yices_context;
      mutable t : Y.yices_type;
      mutable f : Y.yices_type;
      mutable binop: Y.yices_type; (* for uninterp ops *)
      mutable d : (string,Y.yices_var_decl) Hashtbl.t;
      mutable ds : string list ;
      mutable count : int;
      mutable i : int;
    }

    let barrier = "0" 

    let yicesVar me s ty =
      let decl = 
        Misc.do_memo me.d
        (fun () -> 
          let rv = Y.yices_mk_var_decl me.c s ty in
            me.ds <- s::me.ds;rv) () s in
      Y.yices_mk_var_from_decl me.c decl

    let rec isconst = function
	Predicate.PInt(i) -> true
      | _ -> false

    let rec yicesExp me e =
      match e with 
        Predicate.PInt i -> Y.yices_mk_num me.c i 
      | Predicate.Var s -> yicesVar me (Path.unique_name s) me.t
      | Predicate.FunApp (f,e) ->
	  (* we don't need this anymore now that we have WF type checking *)
	  (*if f = "Array.length" | f = "Bigarray.Array2.dim1" || f = "Bigarray.Array2.dim2" then 
	    let (fn, e') = (yicesVar me f me.f, yicesVar me ((function Predicate.Var(s) -> Path.unique_name s | _ -> Predicate.pprint_pexpr Format.std_formatter e; assert false) e) me.ar) in
	      Y.yices_mk_app me.c fn [|e'|]
	  else*)
            let (fn, e') = (yicesVar me f me.f, yicesExp me e) in
              Y.yices_mk_app me.c fn [|e'|]
      | Predicate.Binop (e1,op,e2) ->
          let es' = Array.map (yicesExp me) [|e1;e2|] in
          (match op with 
             Predicate.Plus  -> Y.yices_mk_sum me.c es'
           | Predicate.Minus -> Y.yices_mk_sub me.c es'
           | Predicate.Times ->
	       if (isconst e1) || (isconst e2) then
		 Y.yices_mk_mul me.c es'
	       else
		 let (fd, e1, e2) = (yicesVar me "_MUL" me.binop, yicesExp me e1, yicesExp me e2) in
		   Y.yices_mk_app me.c fd [|e1; e2|]
	   | Predicate.Div ->
	       let (fd, e1, e2) = (yicesVar me "_DIV" me.binop, yicesExp me e1, yicesExp me e2) in
		 Y.yices_mk_app me.c fd [|e1; e2|])
      | Predicate.Field (f, e) ->
          (* pmr: this crucially depends on not having two fields with the same name in the
             same module *)
          let (fn, e') = (yicesVar me (Printf.sprintf "SELECT_%s" f) me.f, yicesExp me e) in
            Y.yices_mk_app me.c fn [|e'|]

    let rec yicesPred me p = 
      match p with 
        Predicate.True -> Y.yices_mk_true me.c
      | Predicate.Not p' -> Y.yices_mk_not me.c (yicesPred me p')
      | Predicate.And (p1,p2) -> Y.yices_mk_and me.c (Array.map (yicesPred me) [|p1;p2|])
      | Predicate.Or (p1,p2) -> Y.yices_mk_or me.c (Array.map (yicesPred me) [|p1;p2|])
      | Predicate.Iff _ as iff -> yicesPred me (Predicate.expand_iff iff)
      | Predicate.Atom (e1,Predicate.Lt,e2) ->
          yicesPred me (Atom (e1, Predicate.Le, Binop(e2,Predicate.Minus,PInt 1)))
      | Predicate.Atom (e1,br,e2) ->
          let e1' = yicesExp me e1 in
          let e2' = yicesExp me e2 in
          (match br with 
             Predicate.Eq -> Y.yices_mk_eq me.c e1' e2' 
           | Predicate.Ne -> Y.yices_mk_diseq me.c e1' e2'
           | Predicate.Gt -> Y.yices_mk_gt me.c e1' e2'
					 | Predicate.Ge -> Y.yices_mk_ge me.c e1' e2'
           | Predicate.Lt -> Y.yices_mk_lt me.c e1' e2'
           | Predicate.Le -> Y.yices_mk_le me.c e1' e2')
   

    let me = 
      let c = Y.yices_mk_context () in
      let _ = if !Clflags.log_queries then Y.yices_enable_log_file "yices.log" else () in
      let t = Y.yices_mk_type c "int" in
      let binop = Y.yices_mk_function_type c [| t; t |] t in
      let f = Y.yices_mk_function_type c [| t |] t in
      let d = Hashtbl.create 37 in
        { c = c; t = t; f = f; binop = binop; d = d; ds = []; count = 0; i = 0; }

    let push p =
      me.count <- me.count + 1;
      if (Bstats.time "consistency" Y.yices_inconsistent me.c) = 1 then
	me.i <- me.i + 1
      else
	begin
	  me.ds <- barrier :: me.ds;
	  Bstats.time "pushing" Y.yices_push me.c;
	  Bstats.time "asserting" (Y.yices_assert me.c) 
          (Bstats.time "creating predicate" (yicesPred me) p)
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
	    Bstats.time "popping" Y.yices_pop me.c
	end

    let reset () =
      Misc.repeat_fn pop me.count;
      Y.yices_reset me.c;
      me.t <- Y.yices_mk_type me.c "int";
      me.binop <- Y.yices_mk_function_type me.c [| me.t; me.t |] me.t;
      me.f <- Y.yices_mk_function_type me.c [| me.t |] me.t;
      me.d <- Hashtbl.create 37;
      me.ds <- [];
      me.count <- 0;
      me.i <- 0

    let unsat () =
      let res = (Bstats.time "consistency" Y.yices_check me.c) = -1 in
        res

    let valid p =
      let _ = push (Predicate.Not p) in
      let rv = unsat () in
      let _ = pop () in
      rv

    let implies (p, q) = 
      let _ = Bstats.time "pushing p" push p in
      let rv = Bstats.time "validating" valid q in
      let _ = Bstats.time "popping" pop () in
      rv

  end

module Prover = YicesProver
