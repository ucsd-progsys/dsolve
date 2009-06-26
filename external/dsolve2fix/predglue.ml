module D = Predicate
module Dp = Path

module F = Ast

let sy_of_path = Dp.unique_name

let f_of_dbop = function
  | D.Plus -> F.Plus
  | D.Minus -> F.Minus
  | D.Times -> F.Times
  | D.Div -> F.Div

let f_of_dbrel = function
  | D.Eq -> F.Eq
  | D.Ne -> F.Ne
  | D.Gt -> F.Gt
  | D.Ge -> F.Ge
  | D.Lt -> F.Lt

let rec fsort_of_dprover_t = function
  | Pparsetree.Array ts -> F.Array (Misc.map_pair fsort_of_dprover ts)
  | Pparsetree.Fun ts -> F.Func (List.map fsort_of_dprover ts)
  | Pparsetree.Pprover_abs ("int") -> F.Int 
  | Pparsetree.Pprover_abs ("bool") -> F.Bool
  | Pparsetree.Pprover_abs s -> F.Unint s

let rec f_of_dexpr = function
  | D.PInt i            -> F.eCon i 
  | D.Var p             -> F.eVar (sy_of_path p)
  | D.FunApp (p, es)    -> F.eApp (sy_of_path p) (List.map f_of_dexpr es)
  | D.Binop (p1, b, p2) -> F.eBin (f_of_dexpr p1) (f_of_dbop b) (f_of_dexpr p2)
  | D.Field (p, e)      -> F.eFld (F.Symbol.of_string (sy_of_path p)) (f_of_dexpr e)
  | D.Ite (b, e1, e2)   -> F.eIte (f_of_dpred b) (f_of_dexpr e1) (f_of_dexpr e2)

and f_of_dpred = function
  | D.True                -> F.pTrue
  | D.Atom (p1, r, p2)    -> F.pAtom (f_of_dexpr p1) (f_of_dbrel r) (f_of_dexpr p2)
  | D.Not p               -> F.pNot (f_of_dpred p)
  | D.And (p1, p2)        -> F.pAnd (f_of_dpred p1) (f_of_dpred p2) 
  | D.Or (p1, p2)         -> F.pOr  (f_of_dpred p1) (f_of_dpred p2) 
  | D.Implies (p1, p2)    -> F.pImplies (f_of_dpred p1) (f_of_dpred p2) 
  | D.Boolexp p           -> F.pBexp (f_of_dpred p)
  | D.Exists (vs, p)      ->
      let vs =
        List.map (fun (p, t) -> (F.Symbol.of_string (sy_of_path p), f_of_dprover_t t)) vs in
      F.pNot (F.pForall (vs, F.pNot (f_of_dpred p)))
  | D.Forall (vs, p)     ->
      let vs =
        List.map (fun (p, t) -> (F.Symbol.of_string (sy_of_path p), fsort_of_dprover_t t)) vs in
      F.pForall (vs, f_of_dpred p)
  | D.Iff (p1, p2) ->
      let p1, p2 = f_of_dpred p1, f_of_dpred p2 in 
      F.pAtom (F.pImp p1 p2) (F.pImp p2 p1)


