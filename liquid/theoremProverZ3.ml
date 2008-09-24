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

open Format
open Predef

module P = Predicate
module C = Common
module F = Frame
module Le = Lightenv

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
let z3_mk_forall c ts ns ze =
  wrap "mk_forall" (Z3.mk_forall c 0 [||] ts ns) ze
let z3_mk_exists c ts ns ze =
  wrap "mk_exists" (Z3.mk_exists c 0 [||] ts ns) ze
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
    val axiom : Predicate.t -> unit
    val set     : F.t Le.t -> P.t list -> bool 
    val valid   : F.t Le.t -> P.t -> bool
    val finish : unit -> unit
    val print_stats : Format.formatter -> unit -> unit
    type sort = Int | Set | Array of sort * sort | Bool | Unint | Func of sort list
  end

module Prover : PROVER = 
  struct

    type sort = Int | Set | Array of sort * sort | Bool | Unint | Func of sort list

    type decl = Vbl of string | Fun of string * int | Barrier
    type var_ast = Const of Z3.ast | Bound of int * sort
    
    type z3_instance = { 
      c                 : Z3.context;
      tint              : Z3.type_ast;
      tun               : Z3.type_ast;
      tset              : Z3.type_ast;
      tbool             : Z3.type_ast;
      vart              : (decl, var_ast) Hashtbl.t;
      funt              : (decl, Z3.const_decl_ast) Hashtbl.t;
      mutable vars      : decl list ;
      mutable count     : int;
      mutable i         : int;
      mutable bnd       : int
    }

    let builtins = [
            ("__tag", Func [Unint; Int]);
            ("_DIV", Func [Int; Int; Int]);
    ]

    let is_select = C.has_prefix "SELECT_"
    let select_type = Func [Int; Int]

    let pprint_sort ppf = function
      | Bool -> fprintf ppf "Bool"
      | Int -> fprintf ppf "Int"
      | Unint -> fprintf ppf "Unint"
      | _ -> assert false

    (* stats *)
    let nb_z3_push  = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop   = ref 0
    let nb_z3_set   = ref 0

    let fresh =
      let x = ref 0 in
      (fun v -> incr x; (v^(string_of_int !x)))

    let rec frame_to_type = function
        F.Fabstract(p, _, _) -> (match p with 
                                | p when p = path_int -> Int
                                | p when set_path p -> Set
                                | _ -> Unint)
      | F.Farrow (_, t1, t2) -> Func (collapse t1 t2)
      | F.Fsum(p, _, _, _) -> (match p with
                                | p when p = path_bool -> Bool
                                | _ -> Unint)
      | _ -> Unint
    and collapse t1 t2 =
      (frame_to_type t1)
      :: (match t2 with F.Farrow (_, t1, t2) -> collapse t1 t2 
                                      | _ -> [frame_to_type t2])
    and set_path p = (Path.name p) = "Myset.set"

    let z3VarType me = function
      | Int -> me.tint
      | Bool -> me.tbool
      | Array _ -> assert false
      | Set -> me.tset
      | Unint -> me.tint (*me.tun*)
      | Func _ -> assert false 

    let z3FunTypes me = function 
      | Func ts -> (match (List.rev_map (z3VarType me) ts) with
                      | x :: [] -> ([], x)
                      | x :: xs -> (List.rev xs, x)
                      | [] -> assert false)
      | _ -> assert false

    let getVarType s env =
      try frame_to_type (Le.find s env) 
        with Not_found -> printf "@[Warning:@ type@ of@ %s@ uninterpretable@ at@ TP@]@." (Path.unique_name s); Unint

    let get_by_name s env =
      List.hd (Le.filterlist (fun p _ -> Path.name p = s) env)

    let getFunType s env =
      if is_select s then select_type else
      try List.assoc s builtins
        with Not_found -> try frame_to_type (get_by_name s env)
          with Failure _ -> failwith (sprintf "@[Could@ not@ type@ function@ %s@ in@ tpz3@]" s)

    let z3Var_memo me s t =
      Misc.do_memo me.vart
      (fun () -> 
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Const (Z3.mk_const me.c sym (z3VarType me t)) in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s)

    let z3Var me s t =
      match z3Var_memo me s t with
          Const v -> v
        | Bound (b, t) -> Z3.mk_bound me.c (me.bnd - b) (z3VarType me t)

    let z3Bind me s t =
      me.bnd <- me.bnd + 1; Hashtbl.replace me.vart (Vbl s) (Bound (me.bnd, t)); me.vars <- (Vbl s) :: me.vars;
      Z3.mk_string_symbol me.c (fresh "z3b")

    let z3Fun me s t k = 
      Misc.do_memo me.funt
      (fun () ->
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let (ts, ret) = z3FunTypes me t in
        let rv  = Z3.mk_func_decl me.c sym (Array.of_list ts) ret in
        me.vars <- (Fun (s,k))::me.vars; rv) 
      () (Fun (s,k))

    let z3App env me s zes =
      let t   = getFunType s env in 
      let k   = List.length zes in
      let zes = Array.of_list zes in
      Z3.mk_app me.c (z3Fun me s t k) zes 

    let rec isConst = function P.PInt i -> true | _ -> false

    let qargs me ps ts = 
      Array.of_list (List.map (fun (p, t) -> z3Bind me (Path.unique_name p) t) (List.combine ps ts))

    let qtypes me ts = Array.of_list (List.map (z3VarType me) ts)  

    let rec z3Exp env me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var me (Path.unique_name s) (getVarType s env)
      | P.FunApp (f,es)         -> z3App env me f (List.map (z3Exp env me) es)
      | P.Binop (e1,P.Plus,e2)  -> Z3.mk_add me.c (Array.map (z3Exp env me) [|e1;e2|]) 
      | P.Binop (e1,P.Minus,e2) -> Z3.mk_sub me.c (Array.map (z3Exp env me) [|e1;e2|]) 
      | P.Binop (e1,P.Times,e2) -> Z3.mk_mul me.c (Array.map (z3Exp env me) [|e1;e2|]) 
      | P.Binop (e1,P.Div,e2)   -> z3App env me "_DIV" (List.map (z3Exp env me) [e1;e2])  
      | P.Field (f, e)          -> z3App env me ("SELECT_"^(Ident.unique_name f)) [(z3Exp env me e)] 
                                   (** REQUIRES: disjoint intra-module field names *)
      | P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred env me e1) (z3Exp env me e2) (z3Exp env me e3)

    and z3Pred env me p = 
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (z3Pred env me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Iff (p, q) -> Z3.mk_iff me.c (z3Pred env me p) (z3Pred env me q)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) -> Z3.mk_eq me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Ne,e2) -> Z3.mk_distinct me.c [|z3Exp env me e1; z3Exp env me e2|]
      | P.Atom (e1,P.Gt,e2) -> Z3.mk_gt me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Ge,e2) -> Z3.mk_ge me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Lt,e2) -> Z3.mk_lt me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Le,e2) -> Z3.mk_le me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Forall (ps, q) -> let fs = [] in mk_quantifier z3_mk_forall env me ps fs q
      | P.Exists (ps, q) -> let fs = [] in mk_quantifier z3_mk_exists env me ps fs q
      | P.Boolexp e -> z3Exp env me e (* must be bool *)

    and mk_quantifier mk env me ps fs q =
      let ts = List.map frame_to_type fs in 
      let args = qargs me ps ts in
      let rv = mk me.c (qtypes me ts) args (z3Pred env me q) in
      me.bnd <- me.bnd - (List.length ps); rv

    let z3Preds env me ps =
      let ps' = List.map (z3Pred env me) ps in
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

    let assert_axiom me p =
      let _ = Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p in
      let _ = Common.cprintf Common.ol_axioms "@[%s@]@." (Z3.ast_to_string me.c p) in
        if unsat me then failwith "Background theory is inconsistent!"

    let push me mkpreds ps =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      if unsat me then me.i <- me.i + 1 else
        let _  = me.vars <- Barrier :: me.vars in
        let p' = Bstats.time "mk preds" (mkpreds me) ps in
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
      let c = Z3.mk_context_x [|("MODEL", "false"); ("PARTIAL_MODELS", "true")|] in
      (* types *)
      let tint = Z3.mk_int_type c in
      let tun = Z3.mk_uninterpreted_type c (Z3.mk_string_symbol c "obj") in
      let tset = Z3.mk_uninterpreted_type c (Z3.mk_string_symbol c "set") in
      let tbool = Z3.mk_bool_type c in
      (* memo tables *)
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      { c = c; tint = tint; tun = tun; tbool = tbool; tset = tset;
      vart = vart; funt = funt; vars = []; count = 0; i = 0; bnd = 0} 

(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    let set env ps = 
      incr nb_z3_set;
      (*let p' = Bstats.time "mk preds" (z3Preds env me) ps in*)
      Bstats.time "z3 push" (push me (z3Preds env)) ps; 
      unsat me 

    let valid env p =
      (*let np' = Bstats.time "mk pred" (z3Pred env me) (P.Not p) in*) 
      let _   = push me (z3Pred env) (P.Not p) in
      let rv  = unsat me in
      let _   = pop me in rv

    let axiom p =
      assert_axiom me (z3Pred Le.empty me p)

    let finish () = 
      (*Bstats.time "Z3 pop"*) pop me; 
      assert (me.count = 0)
 
    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_z3_set !nb_z3_push !nb_z3_pop !nb_z3_unsat

end

