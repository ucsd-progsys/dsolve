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

let force_print s = 
  print_string s; flush stdout

let wrap s f x = f x
 (* let _ = force_print ("Z3 wrap start: "^s^"\n") in 
    let rv = f x in
    let _ = force_print ("Z3 wrap end: "^s^"\n") in 
    rv *)

(***************************************************************)
(*********************** Z3 Wrappers ***************************)
(***************************************************************)

let z3_mk_int_type c = 
  wrap "mk_int_type" (Z3.mk_int_type) c
let z3_mk_string_symbol c s = 
  wrap "mk_string_symbol" (Z3.mk_string_symbol c) s
let z3_mk_const c s ty = 
  wrap "mk_const" (Z3.mk_const c s) ty
let z3_mk_func_decl c s d r = 
  wrap "mk_func_decl" (Z3.mk_func_decl c s d) r
let z3_mk_int c i ty = 
  wrap "mk_int" (Z3.mk_int c i) ty
let z3_mk_add c zea = 
  wrap "mk_add" (Z3.mk_add c) zea
let z3_mk_sub c zea = 
  wrap "mk_sub" (Z3.mk_sub c) zea
let z3_mk_mul c zea = 
  wrap "mk_mul" (Z3.mk_mul c) zea
let z3_mk_ite c ze1 ze2 ze3 = 
  wrap "mk_ite" (Z3.mk_ite c ze1 ze2) ze3
let z3_mk_true c = 
  wrap "mk_true" Z3.mk_true c
let z3_mk_not c ze  = 
  wrap "mk_not" (Z3.mk_not c) ze
let z3_mk_and c zea = 
  wrap "mk_and" (Z3.mk_and c) zea
let z3_mk_or c zea = 
  wrap "mk_or" (Z3.mk_or c) zea
let z3_mk_distinct c zea = 
  wrap "mk_distinct" (Z3.mk_distinct c) zea
let z3_mk_eq c ze1 ze2 = 
  wrap "mk_eq" (Z3.mk_eq c ze1) ze2
let z3_mk_gt c ze1 ze2 = 
  wrap "mk_gt" (Z3.mk_gt c ze1) ze2
let z3_mk_ge c ze1 ze2 = 
  wrap "mk_ge" (Z3.mk_ge c ze1) ze2
let z3_mk_lt c ze1 ze2 = 
  wrap "mk_lt" (Z3.mk_lt c ze1) ze2
let z3_mk_le c ze1 ze2 = 
  wrap "mk_le" (Z3.mk_le c ze1) ze2
let z3_check c = 
  wrap "check" Z3.check c
let z3_ast_to_string c zp = 
  wrap "ast_to_string" (Z3.ast_to_string c) zp
let z3_type_check c zp = 
  wrap "type_check" (Z3.type_check c) zp
let z3_push c = 
  wrap "push" Z3.push c
let z3_assert_cnstr c zp = 
  wrap "assert_cnstr" (Z3.assert_cnstr c) zp
let z3_pop c i = 
  wrap "pop" (Z3.pop c) i 
let z3_mk_context_x a = 
  wrap "mk_context_x" Z3.mk_context_x a
let z3_mk_app c f zea = 
  wrap "mk_app" (Z3.mk_app c f) zea

(***************************************************************)

  module type PROVER = 
  sig
    (* usage: set.valid*.finish *)
    val set     : Predicate.t list -> bool 
    val valid   : Predicate.t -> bool
    val finish : unit -> unit
    val print_stats : Format.formatter -> unit -> unit
  end

module Prover : PROVER = 
  struct
    
    type decl = Vbl of string | Fun of string * int | Barrier
    
    type z3_instance = { 
      c                 : Z3.context;
      tint              : Z3.type_ast;
      vart              : (decl, Z3.ast) Hashtbl.t;
      funt              : (decl, Z3.const_decl_ast) Hashtbl.t;
      mutable vars      : decl list ;
      mutable count     : int;
      mutable i         : int
    }

    (* stats *)
    let nb_z3_push  = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop   = ref 0
    let nb_z3_set   = ref 0

    let fresh =
      let x = ref 0 in
      (fun v -> incr x; (v^(string_of_int !x)))

    let z3Var me s =
      Misc.do_memo me.vart
      (fun () -> 
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Z3.mk_const me.c sym me.tint in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s) 

    let z3Fun me s k = 
      Misc.do_memo me.funt
      (fun () ->
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let rv  = Z3.mk_func_decl me.c sym (Array.make k me.tint) me.tint in
        me.vars <- (Fun (s,k))::me.vars; rv) 
      () (Fun (s,k))

    let z3App me s zes =
      let k   = List.length zes in
      let zes = Array.of_list zes in
      Z3.mk_app me.c (z3Fun me s k) zes 

    let rec isConst = function P.PInt i -> true | _ -> false

    let rec z3Exp me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var me (Path.unique_name s) 
      | P.FunApp (f,es)         -> z3App me f (List.map (z3Exp me) es)
      | P.Binop (e1,P.Plus,e2)  -> Z3.mk_add me.c (Array.map (z3Exp me) [|e1;e2|]) 
      | P.Binop (e1,P.Minus,e2) -> Z3.mk_sub me.c (Array.map (z3Exp me) [|e1;e2|]) 
      | P.Binop (e1,P.Times,e2) -> Z3.mk_mul me.c (Array.map (z3Exp me) [|e1;e2|]) 
      | P.Binop (e1,P.Div,e2)   -> z3App me "_DIV" (List.map (z3Exp me) [e1;e2])  
      | P.Field (f, e)          -> z3App me ("SELECT_"^(Ident.unique_name f)) [(z3Exp me e)] 
                                   (** REQUIRES: disjoint intra-module field names *)
      | P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred me e1) (z3Exp me e2) (z3Exp me e3)

    and z3Pred me p = 
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (z3Pred me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred me) [|p1;p2|])
      | P.Iff _ as iff -> z3Pred me (P.expand_iff iff)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) -> Z3.mk_eq me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Ne,e2) -> Z3.mk_distinct me.c [|z3Exp me e1; z3Exp me e2|]
      | P.Atom (e1,P.Gt,e2) -> Z3.mk_gt me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Ge,e2) -> Z3.mk_ge me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Lt,e2) -> Z3.mk_lt me.c (z3Exp me e1) (z3Exp me e2)
      | P.Atom (e1,P.Le,e2) -> Z3.mk_le me.c (z3Exp me e1) (z3Exp me e2)

    let z3Preds me ps = 
      let ps' = List.map (z3Pred me) ps in
      Z3.mk_and me.c (Array.of_list ps')

    let unsat me =
      let _ = incr nb_z3_unsat in
      let rv = (Bstats.time "Z3 unsat" Z3.check me.c) = Z3.L_FALSE in
      rv

       (* 
    let p2s p = 
      Predicate.pprint Format.str_formatter p;
      Format.flush_str_formatter ()
 
    let z3Pred_wrap me p = 
      let _  = force_print ("z3Pred: in = "^(p2s p)^"\n") in 
      let zp = z3Pred me p in
      let _  = force_print ("z3Pred: out = "^(Z3.ast_to_string me.c zp)^"\n") in
      if not (Z3.type_check me.c zp) then failwith "Dsolve-Z3 type error" else
        zp *)

    let push me p' =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      if unsat me then me.i <- me.i + 1 else
        (* let zp = z3Pred me p in *)
        let _  = me.vars <- Barrier :: me.vars in
        let _  = Z3.push me.c in
        Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p' 

    let rec vpop (cs,s) =
      match s with 
      | [] -> (cs,s)
      | Barrier :: t -> (cs,t)
      | h :: t -> vpop (h::cs,t) 

    let remove_decl me d = 
      match d with 
      | Barrier -> Common.asserts "TheoremProverZ3.remove_decl" false
      | Vbl _ -> Hashtbl.remove me.vart d 
      | Fun _ -> Hashtbl.remove me.funt d

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
      if me.i > 0 then me.i <- me.i - 1 else
        let (cs,vars') = vpop ([],me.vars) in
        let _ = me.vars <- vars' in
        let _ = List.iter (remove_decl me) cs in
        Z3.pop me.c 1 

    let me = 
      let c = Z3.mk_context_x [|("MODEL", "false")|] in
      let tint = Z3.mk_int_type c in
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      { c = c; tint = tint; vart = vart ; funt= funt; 
        vars = []; count = 0; i = 0}

(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    let set ps = 
      incr nb_z3_set;
      let p' = Bstats.time "mk preds" (z3Preds me) ps in 
      Bstats.time "z3 push" (push me) p'; 
      unsat me 

    let valid p =
      let np' = Bstats.time "mk pred" (z3Pred me) (P.Not p) in 
      let _   = push me np' in
      let rv  = unsat me in
      let _   = pop me in rv

    let finish () = 
      Bstats.time "Z3 pop" pop me; 
      assert (me.count = 0)
 
    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_implies_api !nb_z3_push !nb_z3_pop !nb_z3_unsat

end

