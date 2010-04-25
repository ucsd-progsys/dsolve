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
module Le = Liqenv

(***************************************************************)

  module type PROVER = 
  sig
    (* usage: set.valid*.finish *)
    val axiom : F.t Le.t -> Predicate.t -> unit
    val set: F.t Le.t -> P.t list -> bool
    val filter: F.t Le.t -> ('a * P.t) list -> ('a * P.t) list
    val finish: unit -> unit
    val print_stats : Format.formatter -> unit -> unit
    val embed_type : F.t * Parsetree.prover_t -> unit
    val frame_of : Parsetree.prover_t -> F.t
    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list
  end

module Prover : PROVER = 
 struct

    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list

    type decl = Vbl of Path.t | Fun of Path.t * int | Barrier
    type var_ast = Const of Z3.ast | Bound of int * sort
    
    type z3_instance = { 
      c                 : Z3.context;
      tint              : Z3.type_ast;
      tbool             : Z3.type_ast;
      vart              : (decl, var_ast) Hashtbl.t;
      funt              : (decl, Z3.const_decl_ast) Hashtbl.t;
      tydeclt           : (sort, Z3.type_ast) Hashtbl.t;
      mutable vars      : decl list ;
      mutable count     : int;
      mutable bnd       : int;
      mutable frtymap   : (F.t * sort) list;
    }

   (* stats *)
    let nb_z3_push  = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop   = ref 0
    let nb_z3_set   = ref 0

    let fresh =
      let x = ref 0 in
      (fun v -> incr x; (v^(string_of_int !x)))

(***************************************************************************************)
(********************** Typing ************************************************************)
(***************************************************************************************)

    let bofi = Path.mk_ident "_BOFI"
    let iofb = Path.mk_ident "_IOFB"
    let div = Path.mk_ident "_DIV"
    let tag = P.tag_function
    let skolem = P.skolem_function

    let builtins = [
            (tag, Func [Unint "obj"; Int]);
            (div, Func [Int; Int; Int]);
            (iofb, Func [Bool; Int]);
            (bofi, Func [Int; Bool]);
            (skolem, Func [Int; Unint "skolem_constant"])
    ]

    let abs p = F.Fabstract(p, [], Ident.create "", F.empty_refinement)
    let unint = Unint "obj"

    let init_frtymap = [
      (Builtins.uInt, Int); 
      (Builtins.uBool, Bool);
    ]

    let type_to_string t =
      let rec t_rec = function
        | Int -> "int"
        | Bool -> "bool"
        | Unint s -> s
        | Func ts -> "(func " ^ String.concat " " (List.map t_rec ts) ^ ")"
        | Array (s, t) -> "[| " ^ t_rec s ^ "; " ^ t_rec t ^ " |]" in
      t_rec t

    let ast_type_to_string me a =
      Z3.ast_to_string me.c (Z3.type_ast_to_ast me.c a)

    let dump_ast_type me a =
      printf "@[z3%s@]@." 
            (Z3.ast_to_string me.c (Z3.type_ast_to_ast me.c (Z3.get_type me.c a)))

    let dump_ast me a =
      printf "@[%s@]@." (Z3.ast_to_string me.c a)

    let dump_decls me =
      printf "Vars:@.";
      List.iter (function Vbl s -> printf "%s@." (Path.unique_name s) | Barrier -> printf "----@." | _ -> ()) me.vars;
      printf "@."
   
    let rec frame_to_type me = function
      | F.Farrow (_, t1, t2) -> Func (collapse me t1 t2)
      | fr -> snd (List.find (fun (fr', _) -> F.same_shape fr fr') me.frtymap)

    and collapse me t1 t2 =
      (try frame_to_type me t1 with Not_found -> unint)
      :: (match t2 with 
          | F.Farrow (_, t1, t2) -> collapse me t1 t2 
          | _ -> try [frame_to_type me t2] with Not_found -> [unint])
                          
    let rec type_to_frame me = function
      | Func (t :: []) -> type_to_frame me t
      | Func (t :: ts) ->
        (try
          List.assoc t (C.list_assoc_flip me.frtymap)
        with Not_found -> F.Farrow(F.fresh_binder (), type_to_frame me t, type_to_frame me (Func ts)))
      | t -> List.assoc t (C.list_assoc_flip me.frtymap)

    let rec z3VarType me = function
      | Int -> me.tint
      | Bool -> me.tbool
      | Array _ -> assert false
      | Unint _ -> me.tint
      | Func _ -> z3VarType me (Unint ("fun"))

    let z3VarType me t =
      C.do_memo me.tydeclt (fun () -> z3VarType me t) () t

    let rec transl_type me = function
      | Parsetree.Pprover_abs s ->
          (match s with 
          | "int" -> Int
          | "bool" -> Bool
          | s -> Unint s)
      | Parsetree.Pprover_array (t, t') ->
          Array (transl_type me t, transl_type me t')
      | Parsetree.Pprover_fun ts ->
          Func (List.map (transl_type me) ts)

    let z3ArgTypes me = function 
      | Func ts -> (match (List.rev_map (z3VarType me) ts) with
                      | x :: [] -> ([], x)
                      | x :: xs -> (List.rev xs, x)
                      | [] -> assert false)
      | _ -> assert false

    let getVarType me s env =
      let fr = try (Le.find s env) with
                 Not_found -> let p = Path.unique_name s in
                   (eprintf "@[Warning:@ type@ of@ %s@ not@ found@ at@ TP@]@." p; assert false) in
      try frame_to_type me fr
        with Not_found -> unint

    let is_select = C.has_prefix "SELECT_0"
    let select = Path.mk_persistent "SELECT"
    let select_type = Func [Int; Int]
 
    let getFunType me p env =
      if is_select (Path.unique_name p) then select_type
      else try List.assoc p builtins
        with Not_found -> try frame_to_type me (Le.find p env)
          with Not_found -> printf "@[Warning:@ could@ not@ type@ function@ %s@ in@ tpz3@]" (Path.unique_name p); assert false

(***************************************************************************************)
(********************** Vars ***********************************************************)
(***************************************************************************************)

    let z3Var_memo env me s =
      Miscutil.do_memo me.vart
      (fun () -> 
        let t = getVarType me s env in
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Const (Z3.mk_const me.c sym (z3VarType me t)) in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s)

    let z3Var env me s =
      match z3Var_memo env me s with
          Const v -> v
        | Bound (b, t) -> Z3.mk_bound me.c (me.bnd - b) (z3VarType me t)

    let z3Bind me p t =
      me.bnd <- me.bnd + 1; Hashtbl.replace me.vart (Vbl p) (Bound (me.bnd, t)); me.vars <- (Vbl p) :: me.vars;
      Z3.mk_string_symbol me.c (fresh "z3b")

(***************************************************************************************)
(********************** Funs ***********************************************************)
(***************************************************************************************)

    let z3Fun env me p t k = 
      Miscutil.do_memo me.funt
      (fun () ->
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let (ts, ret) = z3ArgTypes me t in
        let rv  = Z3.mk_func_decl me.c sym (Array.of_list ts) ret in
        me.vars <- (Fun (p,k))::me.vars; rv) 
      () (Fun (p,k))

    let rec cast env me ast (t, t') =
      if (t, t') = ("bool", "int") then z3App env me iofb [ast] else
      if (t, t') = ("int", "bool") then z3App env me bofi [ast] else
        assert false

    and z3Cast env me = function
      | (a :: sa, f :: fs) -> 
        let (t, t') = (Z3.get_type me.c a, z3VarType me f) in
        let (st, st') = C.app_pr (ast_type_to_string me) (t, t') in
        let t = if st = st' then a else (cast env me a (st, st')) in
          t :: (z3Cast env me (sa, fs))
      | ([], x :: []) -> []
      | ([], []) -> []
      | _ -> assert false

    and z3App env me p zes =
      let k   = List.length zes in
      let ft  = match getFunType me p env with Func ts -> ts | f -> failwith (sprintf "%s@ ::@ %s" (Path.unique_name p) (type_to_string f)) in
      let cf  = z3Fun env me p (Func ft) k in
      let zes = z3Cast env me (zes, ft) in
      Z3.mk_app me.c cf (Array.of_list zes)

    and z3AppBuiltin env me fes aes =
      z3Cast env me (List.map (z3Exp env me) aes, fes) 

    and z3AppRel env me mk e1 e2 =
      match z3AppBuiltin env me [Int; Int] [e1; e2] with
        [a1; a2] -> mk me.c a1 a2
      | _ -> assert false

(***************************************************************************************)
(********************** Pred/Expr Transl ************************************************************)
(***************************************************************************************)

    and z3Exp env me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var env me s
      | P.FunApp (f,es)         -> z3App env me f (List.map (z3Exp env me) es)
      | P.Binop (e1,P.Plus,e2)  ->
          Z3.mk_add me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Minus,e2) ->
          Z3.mk_sub me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Times,e2) ->
          Z3.mk_mul me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Div,e2)   -> z3App env me div (List.map (z3Exp env me) [e1;e2])  
      | P.Field (f, e)          -> z3App env me (Path.Papply (select, f)) [(z3Exp env me e)]
                                   (** REQUIRES: disjoint intra-module field names *)
      | P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred env me e1) (z3Exp env me e2) (z3Exp env me e3)

    and z3Pred env me p = 
     try
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (z3Pred env me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Implies (p1, p2) -> Z3.mk_implies me.c (z3Pred env me p1) (z3Pred env me p2)
      | P.Iff (p, q) -> Z3.mk_iff me.c (z3Pred env me p) (z3Pred env me q)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) ->
          (*Z3.mk_eq me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_eq e1 e2
      | P.Atom (e1,P.Ne,e2) ->
          Z3.mk_distinct me.c (Array.of_list (z3AppBuiltin env me [Int; Int] [e1; e2]))
      | P.Atom (e1,P.Gt,e2) ->
          (*Z3.mk_gt me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_gt e1 e2
      | P.Atom (e1,P.Ge,e2) ->
          (*Z3.mk_ge me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_ge e1 e2
      | P.Atom (e1,P.Lt,e2) ->
          (*Z3.mk_lt me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_lt e1 e2
      | P.Atom (e1,P.Le,e2) ->
          (*Z3.mk_le me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_le e1 e2
      | P.Forall (ps, q) -> 
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_forall env me ps ts q
      | P.Exists (ps, q) ->
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_exists env me ps ts q
      | P.Boolexp e -> 
          (* shady hack *)
          let a = z3Exp env me e in
          let t = Z3.get_type me.c a in
          let t = ast_type_to_string me t in
          if t = "int" then cast env me a ("int", "bool") else a
     with Failure s -> printf "%s: %a@." s P.pprint p; raise (Failure s)

    and mk_quantifier mk env me ps ts q =
      let args = qargs me ps ts in
      let rv = mk me.c 0 [||] (qtypes me ts) args (z3Pred env me q) in
      me.bnd <- me.bnd - (List.length ps); rv

    and qargs me ps ts = 
      Array.of_list (List.map (fun (p, t) -> z3Bind me p t) (List.combine ps ts))

    and qtypes me ts = Array.of_list (List.map (z3VarType me) ts)  

    let z3Preds env me ps =
      let ps' = List.map (z3Pred env me) ps in
      Z3.mk_and me.c (Array.of_list ps')

(***************************************************************************************)
(********************** Low Level Interface ************************************************************)
(***************************************************************************************)

    let unsat me =
      let _ = incr nb_z3_unsat in
      let rv = (Bstats.time "Z3 unsat" Z3.check me.c) = Z3.L_FALSE in
      rv

    let assert_axiom me p =
      let _ = Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p in
      let _ = Common.cprintf Common.ol_axioms "@[%s@]@." (Z3.ast_to_string me.c p) in
        if unsat me then failwith "Background theory is inconsistent!"

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

    let prep_preds env me ps =
      let ps = List.rev_map (z3Pred env me) ps in
      let _ = me.vars <- Barrier :: me.vars in
      let _ = Z3.push me.c in
        ps

    let push me ps =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      let _  = Z3.push me.c in
        List.iter (fun p -> Z3.assert_cnstr me.c p) ps

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
        Z3.pop me.c 1 

    let valid me p =
      let _ = push me [(Z3.mk_not me.c p)] in
      let rv = unsat me in
      let _ = pop me in
        rv

    let me = 
      let c = Z3.mk_context_x [|("MODEL", "false"); ("PARTIAL_MODELS", "true")|] in
      (* types *)
      let tint = Z3.mk_int_type c in
      let tbool = Z3.mk_bool_type c in
      (* memo tables *)
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      let tydeclt = Hashtbl.create 37 in
      { c = c; tint = tint; tbool = tbool; tydeclt = tydeclt; frtymap = init_frtymap;
        vart = vart; funt = funt; vars = []; count = 0; bnd = 0}

(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    let clean_decls () =
      let (cs,vars') = vpop ([],me.vars) in
      let _ = me.vars <- vars' in
      let _ = List.iter (remove_decl me) cs in ()

    let set env ps =
      let _   = Hashtbl.remove me.vart (Vbl C.qual_test_var) in
      let ps  = prep_preds env me ps in
      let _   = push me ps in
        unsat me

    let filter env ps =
      let rv =
        let ps = List.rev_map (fun (q, p) -> (z3Pred env me p, (q, p))) ps in
        List.filter (fun (p, _) -> valid me p) ps in
      let _ = pop me in
      let _ = clean_decls () in
        snd (List.split rv)

    let finish () =
      pop me

    let axiom env p =
      assert_axiom me (z3Pred env me p)

    let embed_type (fr, t) =
      me.frtymap <- (fr, transl_type me t) :: me.frtymap

    let frame_of pt =
      try
        type_to_frame me (transl_type me pt)
      with Not_found -> raise (Failure (C.prover_t_to_s pt))

    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_z3_set !nb_z3_push !nb_z3_pop !nb_z3_unsat

    let _ = 
      let (x, y) = C.app_pr Path.mk_ident ("x", "y") in
      let bol = Parsetree.Pprover_abs "bool" in
      let itn = Parsetree.Pprover_abs "int" in
      let func s x = P.FunApp(s, [P.Var x]) in
        axiom Le.empty
          (P.Forall ([(x, bol)], P.Iff(P.Atom(func iofb x, P.Eq, P.PInt(1)), P.Boolexp(P.Var x))));
        axiom Le.empty 
          (P.Forall ([(x, itn)], P.Iff(P.Boolexp(func bofi x), P.Atom(P.Var x, P.Eq, P.PInt(1)))));
end
    

