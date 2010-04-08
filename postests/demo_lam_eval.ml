(* DSOLVE -dontgenmlq *)

let show x = ()

(**********************************************************************)
(****************** Datatype Definition *******************************)
(**********************************************************************)

type expr = 
  | Const of int
  | Plus of expr * expr
  | Lam of string * expr
  | Pair of expr * expr
  | Var of string
  | App of expr * expr
  | Fst of expr
  | Snd of expr

(*********************************************************************)
(********************* The "Value" Measure ***************************)
(*********************************************************************)

let rec ok ex = 
  match ex with
  | Const i       -> true 
  | Lam  (x,e)    -> true 
  | Pair (e1, e2) -> (ok e1) && (ok e2)
  | Var  x        -> false 
  | Plus (e1, e2) -> false 
  | App  (e1, e2) -> false 
  | Fst  e        -> false 
  | Snd  e        -> false 


(********************************************************************)
(************** Evaluator *******************************************)
(********************************************************************)

(* A "value" is just: {v: expr| (ok v)} *)

let rec eval sto = function
  | Const i -> 
     (sto, Const i)

  | Var x -> 
     (sto, List.assoc x sto)

  | Plus (e1, e2) ->
      let _, Const i1 = eval sto e1 in
      let _, Const i2 = eval sto e2 in
      (sto, Const (i1 + i2))

  | App (e1, e2) ->
      let (sto1, Lam (x, e)) = eval sto e1 in
      let (_   , v2)         = eval sto e2 in
      eval ((x, v2)::sto1) e

  | Lam (x, e) ->
      (sto, Lam (x, e))

  | Pair (e1, e2) ->
      let _, v1 = eval sto e1 in
      let _, v2 = eval sto e2 in
      (sto, Pair (v1, v2))

  | Fst e ->
      let _,y = eval sto e in
      (match y with
       | Pair (v1,v2) -> (sto, v1))

  | Snd e ->
      let _, y = eval sto e in
      (match y with
       | Pair (v1,v2) -> (sto, v2))


(*****************************************************************)
(**************** Value Checker **********************************)
(*****************************************************************)

let rec check_value = function
  | Const _ -> 
      ()
  | Lam (_,_) -> 
      ()
  | Pair (v1, v2) ->
      check_value v1; check_value v2
  | Plus (_, _) ->
      assert (0=1); assert false
  | Var x ->
      assert (0=1); assert false
  | App (_, _) ->
      assert (0=1); assert false
  | Fst e ->
      assert (0=1); assert false
  | Snd e ->
      assert (0=1); assert false

(*********************************************************************)
(********************* Wrapped Evaluator *****************************)
(*********************************************************************)

let eval sto e = 
  let s, v = eval sto e in
  let _    = check_value v in
  (s, v)

(*********************************************************************)
(******************** Unit Tests *************************************) 
(*********************************************************************)

let e1 = Const 10
let e2 = Lam ("f", Lam ("g", Lam ("x", App (Var "f", App (Var "g", Var "x")))))
let e3 = Lam ("x", Plus (Var "x", e1))
let e4 = App ((App (e2, e3)), e3)
let e5 = Pair ((App (e4, Const 0)), (App (e4, Const 100)))
let e6 = Fst e5
let e7 = Snd e5

let _, v1  = eval [] e1
let _, v2  = eval [] e2
let _, v3  = eval [] e3
let _, v4  = eval [] e4
let _, v5  = eval [] e5
let _, v6  = eval [] e6
let _, v7  = eval [] e7

