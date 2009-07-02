module D = Predicate
module Pa = Path

module F = Ast
module Fp = F.Predicate
module Fe = F.Expression
module Fs = F.Sort

let str_to_path = Hashtbl.create 37
let path_to_str = Hashtbl.create 37

let str_of_path p =
  Common.do_bimemo path_to_str str_to_path Pa.unique_name p p
let sy_of_path p = F.Symbol.of_string (str_of_path p)

let path_of_str s = Hashtbl.find str_to_path s
let path_of_sy s = path_of_str (F.Symbol.to_string s)

let sy_of_persistent p = F.Symbol.of_string (Path.name p)

let f_of_dbop = function
  | D.Plus -> F.Plus
  | D.Minus -> F.Minus
  | D.Times -> F.Times
  | D.Div -> F.Div

let d_of_fbop = function
  | F.Plus -> D.Plus
  | F.Minus -> D.Minus
  | F.Times -> D.Times
  | F.Div -> D.Div

let f_of_dbrel = function
  | D.Eq -> F.Eq
  | D.Ne -> F.Ne
  | D.Gt -> F.Gt
  | D.Ge -> F.Ge
  | D.Lt -> F.Lt
  | D.Le -> F.Le

let d_of_fbrel = function
  | F.Eq -> D.Eq
  | F.Ne -> D.Ne
  | F.Gt -> D.Gt
  | F.Ge -> D.Ge
  | F.Lt -> D.Lt
  | F.Le -> D.Le

let rec fsort_of_dprover_t = function
  | Parsetree.Pprover_array (t1, t2) ->
      Fs.Array (fsort_of_dprover_t t1, fsort_of_dprover_t t2)
  | Parsetree.Pprover_fun ts ->
      Fs.Func (List.map fsort_of_dprover_t ts)
  | Parsetree.Pprover_abs ("int") -> Fs.Int 
  | Parsetree.Pprover_abs ("bool") -> Fs.Bool
  | Parsetree.Pprover_abs s -> Fs.Unint s

let rec dprover_t_of_fsort = function
  | Fs.Int -> Parsetree.Pprover_abs ("int")
  | Fs.Bool -> Parsetree.Pprover_abs ("bool")
  | Fs.Unint s -> Parsetree.Pprover_abs s
  | Fs.Array (t1, t2) ->
      Parsetree.Pprover_array (dprover_t_of_fsort t1, dprover_t_of_fsort t2)
  | Fs.Func ts -> Parsetree.Pprover_fun (List.map dprover_t_of_fsort ts)

let rec f_of_dexpr = function
  | D.PInt i            -> F.eCon (F.Constant.Int i) 
  | D.Var p             -> F.eVar (sy_of_path p)
  | D.FunApp (p, es)    -> F.eApp (sy_of_path p, List.map f_of_dexpr es)
  | D.Binop (p1, b, p2) -> F.eBin (f_of_dexpr p1, f_of_dbop b, f_of_dexpr p2)
  | D.Field (p, e)      -> F.eFld (sy_of_path p, f_of_dexpr e)
  | D.Ite (b, e1, e2)   -> F.eIte (f_of_dpred b, f_of_dexpr e1, f_of_dexpr e2)

and f_of_dpred = function
  | D.True                -> F.pTrue
  | D.Atom (p1, r, p2)    -> F.pAtom (f_of_dexpr p1, f_of_dbrel r, f_of_dexpr p2)
  | D.Not p               -> F.pNot (f_of_dpred p)
  | D.And (p1, p2)        -> F.pAnd [f_of_dpred p1; f_of_dpred p2] 
  | D.Or (p1, p2)         -> F.pOr  [f_of_dpred p1; f_of_dpred p2]
  | D.Implies (p1, p2)    -> F.pImp (f_of_dpred p1, f_of_dpred p2) 
  | D.Boolexp p           -> F.pBexp (f_of_dexpr p)
  | D.Exists (vs, p)      ->
      let vs =
        List.map (fun (p, t) -> (F.Symbol.of_string (str_of_path p), fsort_of_dprover_t t)) vs in
      F.pNot (F.pForall (vs, F.pNot (f_of_dpred p)))
  | D.Forall (vs, p)     ->
      let vs =
        List.map (fun (p, t) -> (F.Symbol.of_string (str_of_path p), fsort_of_dprover_t t)) vs in
      F.pForall (vs, f_of_dpred p)
  | D.Iff (p1, p2) ->
      let p1, p2 = f_of_dpred p1, f_of_dpred p2 in 
      F.pAnd [F.pImp (p1, p2); F.pImp (p2, p1)]

let rec d_of_fexpr e =
  match Fe.unwrap e with  
  | F.Con (F.Constant.Int i) -> D.PInt i
  | F.Var v                  -> D.Var (path_of_sy v)
  | F.App (fn, rgs)          -> D.FunApp (path_of_sy fn, List.map d_of_fexpr rgs)
  | F.Bin (e1, bop, e2)      -> D.Binop (d_of_fexpr e1, d_of_fbop bop, d_of_fexpr e2)
  | F.Ite (p, e1, e2)        -> D.Ite (d_of_fpred p, d_of_fexpr e1, d_of_fexpr e2)
  | F.Fld (fn, e)            -> D.Field (path_of_sy fn, d_of_fexpr e)

and d_of_fpred p =
  match Fp.unwrap p with
  | F.True                   -> D.True
  | F.False                  -> D.Not (D.True)
  | F.And ps                 ->
      List.fold_left (fun a p -> D.And (d_of_fpred p, a)) D.True ps
  | F.Or ps                  ->
      List.fold_left (fun a p -> D.Or (d_of_fpred p, a)) (D.Not (D.True)) ps
  | F.Not p                  -> D.Not (d_of_fpred p)
  | F.Imp (p1, p2)           -> D.Implies (d_of_fpred p1, d_of_fpred p2)
  | F.Bexp e                 -> D.Boolexp (d_of_fexpr e)
  | F.Atom (e1, rel, e2)     ->
      D.Atom (d_of_fexpr e1, d_of_fbrel rel, d_of_fexpr e2)
  | F.Forall (ss, p)         ->
      let ss =
        List.map (fun (s, st) -> (path_of_sy s, dprover_t_of_fsort st)) ss in 
      D.Forall (ss, d_of_fpred p)
