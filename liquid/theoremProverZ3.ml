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

(***************************************************************)

  module type PROVER = 
  sig
    (* usage: set.valid*.finish *)
    val axiom : F.t Le.t -> Predicate.t -> unit
    val set     : F.t Le.t -> P.t list -> bool 
    val valid   : F.t Le.t -> P.t -> bool
    val finish : unit -> unit
    val print_stats : Format.formatter -> unit -> unit
    val embed_type : F.t * Parsetree.prover_t -> unit
    val frame_of : Parsetree.prover_t -> F.t
    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list
  end

module Prover : PROVER = 
 struct

    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list

    type decl = Vbl of Path.t | Fun of string * int | Barrier
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
      mutable i         : int;
      mutable bnd       : int;
      mutable v         : Z3.ast option;
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

    let get_by_name s env =
      List.hd (Le.filterlist (fun p _ -> Path.name p = s) env)

(***************************************************************************************)
(********************** Typing ************************************************************)
(***************************************************************************************)

    let builtins = [
            ("__tag", Func [Unint "obj"; Int]);
            ("_DIV", Func [Int; Int; Int]);
            ("_IOFB", Func [Bool; Int]);
    ]

    let abs p = F.Fabstract(p, [], F.empty_refinement)
    let unint = Unint "obj"

    let init_frtymap = [
      (Builtins.uInt, Int); 
      (Builtins.uBool, Bool);
      (Builtins.uUnit, Bool);
    ]

    let type_to_string t =
      let rec t_rec = function
        | Int -> "int"
        | Bool -> "bool"
        | Unint s -> s
        | Func ts -> "(func " ^ String.concat " " (List.map t_rec ts) ^ ")"
        | _ -> assert false in
      t_rec t
   
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
      C.do_memo me.tydeclt (z3VarType me) t t

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
      try frame_to_type me (Le.find s env) 
        with Not_found ->
          let p = Path.unique_name s in
          printf "@[Warning:@ type@ of@ %s@ uninterpretable@ at@ TP@]@." p; unint

    let is_select = C.has_prefix "SELECT_"
    let select_type = Func [Int; Int]
 
    let getFunType me s env =
      if is_select s then select_type
      else try List.assoc s builtins
        with Not_found -> try frame_to_type me (get_by_name s env)
          with Not_found -> printf "@[Warning:@ could@ not@ type@ function@ %s@ in@ tpz3@]" s; unint

(***************************************************************************************)
(********************** Vars ************************************************************)
(***************************************************************************************)

    let z3Var_memo env me s =
      Misc.do_memo me.vart
      (fun () -> 
        let t = getVarType me s env in
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Const (Z3.mk_const me.c sym (z3VarType me t)) in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s)

    let maybe_remap_v env me s =
      match me.v with
      | Some c -> c
      | None -> 
          let rv = Z3.mk_const me.c (Z3.mk_string_symbol me.c (fresh "AA"))
            (z3VarType me (getVarType me s env)) in
            me.v <- Some rv; rv

    let z3Var env me s =
      if s = C.qual_test_var then maybe_remap_v env me s
      else match z3Var_memo env me s with
          Const v -> v
        | Bound (b, t) -> Z3.mk_bound me.c (me.bnd - b) (z3VarType me t)

    let z3Bind me p t =
      me.bnd <- me.bnd + 1; Hashtbl.replace me.vart (Vbl p) (Bound (me.bnd, t)); me.vars <- (Vbl p) :: me.vars;
      Z3.mk_string_symbol me.c (fresh "z3b")

(***************************************************************************************)
(********************** Funs ************************************************************)
(***************************************************************************************)

    let z3Fun env me s k = 
      Misc.do_memo me.funt
      (fun () ->
        let t   = getFunType me s env in
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let (ts, ret) = z3ArgTypes me t in
        let rv  = Z3.mk_func_decl me.c sym (Array.of_list ts) ret in
        me.vars <- (Fun (s,k))::me.vars; rv) 
      () (Fun (s,k))

    let cast me ast = function
      (*| (me.tbool, me.tint) -> Z3.mk_app (z3FunType "_BOFI" ) *)
      | _ -> assert false

    let rec z3Cast me = function
      | (f :: fs, a :: sa) -> 
        let (t, t') = (Z3.get_type me.c a, z3VarType me f) in
        let t = if t != t' then (cast me a (t, t')) else a in
          t :: (z3Cast me (fs, sa))
      | ([], []) -> []
      | _ -> assert false

    let z3App env me s zes =
      let k   = List.length zes in
      let zes = Array.of_list zes in
      let cf  = z3Fun env me s k in
      let rv  = Z3.mk_app me.c cf zes in
        (*if Z3.type_check me.c rv then rv else
          (*let ft = match getFunType env s with Func ts -> ts | _ -> assert false in
          let zes = z3Cast (ft, zes) in*)
            Z3.mk_app me.c cf zes*) rv

(***************************************************************************************)
(********************** Pred/Expr Transl ************************************************************)
(***************************************************************************************)

    let rec z3Exp env me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> let _ = printf "%s@." (type_to_string (getVarType me s env)) in z3Var env me s
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
      | P.And (p1,p2) -> printf "and@."; Z3.mk_and me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Iff (p, q) -> printf "asdfadfasdf@."; Z3.mk_iff me.c (z3Pred env me p) (z3Pred env me q)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) -> printf "eq@."; Z3.mk_eq me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Ne,e2) -> Z3.mk_distinct me.c [|z3Exp env me e1; z3Exp env me e2|]
      | P.Atom (e1,P.Gt,e2) -> Z3.mk_gt me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Ge,e2) -> Z3.mk_ge me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Lt,e2) -> Z3.mk_lt me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Atom (e1,P.Le,e2) -> Z3.mk_le me.c (z3Exp env me e1) (z3Exp env me e2)
      | P.Forall (ps, q) -> 
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_forall env me ps ts q
      | P.Exists (ps, q) ->
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_exists env me ps ts q
      | P.Boolexp e -> z3Exp env me e (* must be bool *)

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

    let push me mkpreds ps =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      if unsat me then me.i <- me.i + 1 else
        let _  = me.vars <- Barrier :: me.vars in
        let p' = Bstats.time "mk preds" (mkpreds me) ps in
        let _  = Z3.push me.c in
        Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p' 

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
      if me.i > 0 then me.i <- me.i - 1 else
        Z3.pop me.c 1 

    let me = 
      let c = Z3.mk_context_x [|("MODEL", "false"); ("PARTIAL_MODELS", "true")|] in
      (* types *)
      let tint = Z3.mk_int_type c in
      let tbool = Z3.mk_bool_type c in
      (* memo tables *)
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      let tydeclt = Hashtbl.create 37 in
      { c = c; tint = tint; tbool = tbool; v = None; tydeclt = tydeclt; frtymap = init_frtymap;
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

    let axiom env p =
      assert_axiom me (z3Pred env me p)

    let embed_type (fr, t) =
      me.frtymap <- (fr, transl_type me t) :: me.frtymap

    let frame_of pt =
      try
        type_to_frame me (transl_type me pt)
      with Not_found -> raise (Failure (C.prover_t_to_s pt))

    let finish () = 
      (*Bstats.time "Z3 pop"*) pop me; 
      me.v <- None;
      assert (me.count = 0)
 
    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_z3_set !nb_z3_push !nb_z3_pop !nb_z3_unsat

end
