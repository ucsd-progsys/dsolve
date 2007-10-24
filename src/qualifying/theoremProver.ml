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
    
  end

module M = Message

module SimplifyProver : PROVER = 
  struct 
(***************************************************************************)
(**** Interface to Simplify, run as a server and queries are piped in ******)
(***************************************************************************)

let fixed_simplify_axioms = ref 	    
[ "(BG_PUSH (FORALL (x y) (EQ (select (addrOf x y) 0) x)))" ;
  "(BG_PUSH (FORALL (x y d1) (IMPLIES (EQ (foffset x d1) (foffset y d1)) (EQ  x y))))\n" ;
  "(BG_PUSH (FORALL (x y) (NEQ (addrOf x y) 0)))" ;
  "(BG_PUSH (FORALL (x y) (EQ (* (Div x y) y) x )) ) " ;
  "(BG_PUSH (FORALL (x) (NEQ (_STRINGCONSTANT x) 0 ))) " ;
  "(BG_PUSH (FORALL (s t x ) (IMPLIES (EQ s (add t x)) (EQ (in s x) 1))))";
  "(BG_PUSH (FORALL (s x) (IMPLIES (EQ s (emptyset )) (NEQ (in s x) 1))))"]


(***************************************************************************)
(************ utilities for piping queries *********************************)
(***************************************************************************)
exception ChannelException

let simplifyServer = ref None
let simplify_stack_counter = ref 0
let current_simplify_stack = ref [] 
let simplifyServerCount = ref 0 
let simplifyCacheCount = ref 0 
let simplify_query_flag = ref false 

let reset_alarm () = ()

let set_alarm () = ()

(* may raise ChannelException *)
let secure_output_string oc s = 
  try set_alarm (); output_string oc s; flush oc; reset_alarm ()
  with ChannelException -> reset_alarm (); raise ChannelException

(* may raise ChannelException *)
let secure_input_line ic =  
  set_alarm ();
  try
    let rv = input_line ic in 
    reset_alarm (); rv
  with ChannelException ->
    reset_alarm () ;
    raise ChannelException

let getServer () =
  match !simplifyServer with Some (a,b,_) -> (a,b) 
  | None -> 
      M.msg_string M.Debug "Forking Simplify process..." ;
      let ic,oc,ec = Unix.open_process_full "Simplify -nosc" (Unix.environment ()) in
      simplifyServer := Some(ic,oc,ec) ;
      List.iter (fun x -> secure_output_string oc (x^"\n")) !fixed_simplify_axioms ;
      M.msg_string M.Debug "done!\n";
      flush stdout ;
      at_exit (fun () -> ignore (Unix.close_process_full (ic,oc,ec)););
      (ic,oc)
and kill_simplify_server () = 
  match !simplifyServer with None -> ()
    | Some (ic,oc,ec) -> 
        try
	  M.msg_string M.Normal "Killing simplify server...";
	  ignore(Sys.command "killall Simplify");
	  simplifyServer := None;
	  M.msg_string M.Normal "Done killing ..." ;
	with _ -> failwith " failed to kill simplify !"

let restart_simplify () = 
  M.msg_string M.Normal "restarting simplify ...";
  kill_simplify_server ();
  let (ic,oc) = getServer () in
    try
      List.iter (secure_output_string oc) (List.rev !current_simplify_stack)
    with ChannelException -> failwith "Simplify fails on restart!"

let channel_exn_restart s = 
  M.msg_string M.Error ("ChannelException in "^s); 
  restart_simplify ()

let is_substring s subs =
  let reg = Str.regexp subs in
  try ignore(Str.search_forward reg s 0); true
  with Not_found -> false

let rec isValid ic = 
  let line = secure_input_line ic in
  if is_substring line "Bad input" then (M.msg_string M.Error "Simplify poisoned!"; exit 1)
  else if String.contains line 'V' then true
  else if String.contains line 'I' then false
  else isValid ic

(********************************************************************************)
(***************** Converting to the Simplify Format ****************************)
(********************************************************************************)


let convert_rel r = 
  match r with Eq -> "EQ" | Ne -> "NEQ" | Le -> "<=" | Lt -> "<" | Gt -> ">" | Ge -> ">="

let convert_op o = 
  match o with Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

let name_substitutions =
  [(Str.regexp_string "'", "ptick");
   (Str.regexp_string "_", "undscore");
  ]

let convert_var_name path =
  let name = Path.unique_name path in
    List.fold_left (fun n (rex, rpl) -> Str.global_replace rex rpl n)
      name name_substitutions

let rec convert_exp e =
  match e with
    PInt i -> string_of_int i  (* pmr: ask Ranjit why there was an assertion here *)
  | Var x -> convert_var_name x
  | Pvar (x,i) -> Printf.sprintf "%sprime%d" (convert_var_name x) i
  | Binop (e1,op,e2) -> Printf.sprintf "(%s %s %s)" (convert_op op) (convert_exp e1) (convert_exp e2)
  | FunApp (f,e) -> Printf.sprintf "(%s %s)" f (convert_exp e)

let rec convert_pred p = 
  match p with 
    True -> "(EQ 0 0)"  
  | Atom (e1,r,e2) -> Printf.sprintf "(%s %s %s)" (convert_rel r) (convert_exp e1) (convert_exp e2)
  | Not p -> Printf.sprintf "(NOT %s)" (convert_pred p) 
  | And (p1,p2) -> Printf.sprintf "(AND %s %s)" (convert_pred p1) (convert_pred p2)
  | Or (p1,p2) -> Printf.sprintf "(OR %s %s)" (convert_pred p1) (convert_pred p2)

(********************************************************************************)
(************************** Unbreaking Division *********************************)
(********************************************************************************)

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


(********************************************************************************)
(**************************** Issuing Queries ***********************************)
(********************************************************************************)

let push pred =
  let fixed = fixdiv pred in
  let s = Printf.sprintf "(BG_PUSH %s) \n" (convert_pred fixed) in
  let _ = current_simplify_stack := s :: !current_simplify_stack in
  let _ = simplify_stack_counter := !simplify_stack_counter + 1 in
  let (_,oc) = getServer () in
  try secure_output_string oc s;flush oc ;     
  with ChannelException -> channel_exn_restart "simplify_assume"

let pop () = 
  let _ = 
    if (!simplify_stack_counter = 0) then failwith "bad simplify stack!"
    else simplify_stack_counter := !simplify_stack_counter - 1 in
  let _ = current_simplify_stack := List.tl !current_simplify_stack in
  let (_,oc) = getServer () in
  let s = "(BG_POP)\n" in
  try secure_output_string oc s; flush oc
  with ChannelException -> channel_exn_restart "simplify_pop" 

let valid p = 
  let s = convert_pred p in
  let rec qe flag = 
    let _ = flush stdout; flush stderr in 
    try
      let ic,oc = getServer () in
      secure_output_string oc s;flush oc; isValid ic
    with ChannelException when flag -> failwith "Simplify fails again!" 
       | ChannelException -> restart_simplify (); qe true
       | e -> 
         failwith (Format.fprintf Format.str_formatter
           "Simplify raises %s for %a. Check that Simplify is in your path \n" 
           (Printexc.to_string e) Predicate.pprint p; Format.flush_str_formatter ()) in
  qe false

let reset () = 
  Misc.repeat_fn pop (!simplify_stack_counter)

let implies p q = 
  let _  = push p in
  let rv = valid q in
  let _  = pop () in
  rv


end

(*
module Y = Oyices

module YicesProver  = 
  struct
    
    type yices_instance = { 
      c : Y.yices_context;
      t : Y.yices_type;
      d : (string,Y.yices_var_decl) Hashtbl.t;
      mutable ds : string list ;
      mutable count : int
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
        { c = c; t = t; d = d; ds = []; count = 0 }

    let push p =
      me.count <- me.count + 1;
      me.ds <- barrier :: me.ds;
      Y.yices_push me.c;
      Y.yices_assert me.c (yicesPred me p)
      
    let rec vpop (cs,s) =
      match s with [] -> (cs,s)
      | h::t when h = barrier -> (cs,t)
      | h::t -> vpop (h::cs,t)

    let pop () = 
      let (cs,ds') = vpop ([],me.ds) in
      me.ds <- ds';
      me.count <- me.count - 1;
      List.iter (Hashtbl.remove me.d) cs;
      Y.yices_pop me.c

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
*)

module Prover = SimplifyProver

