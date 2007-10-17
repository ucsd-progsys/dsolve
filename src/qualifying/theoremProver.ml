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

    (* implies p q = true iff predicate p (provably) implies predicate q *)
    val implies : Predicate.t -> Predicate.t -> bool

    (* approximate total time spent in (underlying) theorem prover queries *)
    val querytime : unit -> float
    
  end


module Y = Oyices

module YicesProver  = 
  struct
    
    type yices_instance = { 
      c : Y.yices_context;
      t : Y.yices_type;
			ar: Y.yices_type;
      f : Y.yices_type;
			binop: Y.yices_type; (* for uninterp ops *)
      d : (string,Y.yices_var_decl) Hashtbl.t;
      mutable ds : string list ;
      mutable count : int;
      mutable i : int;
      mutable qtime : float;
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
      | Predicate.Pvar (s,i) -> yicesVar me
          (Printf.sprintf "%sprime%d" (Path.unique_name s) i) me.t
      | Predicate.FunApp (f,e) ->
	  (* huge hack alert *)
	  if f = "Array.length" then 
	    let (fn, e') = (yicesVar me f me.f, yicesVar me ((function Predicate.Var(s) -> Path.unique_name s | _ -> Predicate.pprint_pexpr Format.std_formatter e; assert false) e) me.ar) in
	      Y.yices_mk_app me.c fn [|e'|]
	  else
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


    let rec yicesPred me p = 
      match p with 
        Predicate.True -> Y.yices_mk_true me.c
      | Predicate.Not p' -> Y.yices_mk_not me.c (yicesPred me p')
      | Predicate.And (p1,p2) -> Y.yices_mk_and me.c (Array.map (yicesPred me) [|p1;p2|])
      | Predicate.Or (p1,p2) -> Y.yices_mk_or me.c (Array.map (yicesPred me) [|p1;p2|])
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

let rec fixdiv p = 
   let expr_isdiv = 
       function Predicate.Binop(_, Predicate.Div, _) -> true
                | _ -> false in 
   let pull_const =
       function Predicate.PInt(i) -> i
                | _ -> 1 in
   let pull_divisor =
       function Predicate.Binop(_, Predicate.Div, d1) ->
                pull_const d1 
                | _ -> 1 in
   let rec apply_mult m e =
       match e with
           Predicate.Binop(n, Predicate.Div, Predicate.PInt(d)) ->
               let _ = assert ((m/d) * d = m) in
               Predicate.Binop(Predicate.PInt(m/d), Predicate.Times, n) 
           | Predicate.Binop(e1, rel, e2) ->
               Predicate.Binop(apply_mult m e1, rel, apply_mult m e2) 
           | Predicate.PInt(i) -> Predicate.PInt(i*m)
           | e -> Predicate.Binop(Predicate.PInt(m), Predicate.Times, e)
           in
   let rec pred_isdiv = 
       function Predicate.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
                | Predicate.And(p, p') -> (pred_isdiv p) || (pred_isdiv p')
                | Predicate.Or(p, p') -> (pred_isdiv p) || (pred_isdiv p')
                | Predicate.True -> false
                | Predicate.Not p -> pred_isdiv p in
   let calc_cm e1 e2 =
       pull_divisor e1 * pull_divisor e2 in
   if pred_isdiv p then
   match p with
       Predicate.Atom(e, r, e') -> 
                let m = calc_cm e e' in
                let e'' = Predicate.Binop(e', Predicate.Minus, Predicate.PInt(1)) in
                let bound (e, r, e', e'') = 
                    Predicate.And(Predicate.Atom(apply_mult m e, Predicate.Gt, apply_mult m e''),
                                  Predicate.Atom(apply_mult m e, Predicate.Le, apply_mult m e'))
                in
                (match (e, r, e') with
                  (Predicate.Var v, Predicate.Eq, e') ->
                    bound (e, r, e', e'')
                  | (Predicate.PInt v, Predicate.Eq, e') ->
                    bound (e, r, e', e'')
                  | _ -> p) 
       | Predicate.And(p1, p2) -> 
                let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
                let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
                Predicate.And(p1, p2)      
       | Predicate.Or(p1, p2) ->
                let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
                let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
                Predicate.Or(p1, p2) 
       | Predicate.Not p1 -> Predicate.Not(fixdiv p1) 
       | p -> p
   else p
   

    let me = 
      let c = Y.yices_mk_context () in
      let t = Y.yices_mk_type c "int" in
			let ar = Y.yices_mk_type c "a' array" in
      (*let unknown = Y.yices_mk_type c "unk" in*)
			(* need a blind uninterp function again eventually *)
			let binop = Y.yices_mk_function_type c [| t; t |] t in
      let f = Y.yices_mk_function_type c [| ar |] t in
      let d = Hashtbl.create 37 in
        { c = c; t = t; ar = ar; f = f; binop = binop; d = d; ds = []; count = 0; i = 0; qtime = 0.0 }

    let push p =
      let start = Sys.time () in
        me.count <- me.count + 1;
        if Y.yices_inconsistent me.c = 1 then
	  me.i <- me.i + 1
        else
	  begin
	    me.ds <- barrier :: me.ds;
	    Y.yices_push me.c;
	    Y.yices_assert me.c 
              (let p' = fixdiv p in 
                 (*let _ = if (fixdiv p) != p then
                   (Predicate.pprint Format.std_formatter p; 
                   Predicate.pprint Format.std_formatter p') 
                   else () in *)
                 yicesPred me p')
	  end;
        me.qtime <- me.qtime +. (Sys.time () -. start)
      
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
      let start = Sys.time () in
      let res = Y.yices_check me.c = -1 in
        me.qtime <- me.qtime +. (Sys.time () -. start);
        res

    let valid p =
      let _ = push (Predicate.Not p) in
      let rv = unsat () in
      let _ = pop () in
      rv

    let implies p q = 
      let _ = push p in
      let rv = valid q in
      let _ = pop () in
      rv

    let querytime () =
      me.qtime
  end

module Prover = YicesProver
