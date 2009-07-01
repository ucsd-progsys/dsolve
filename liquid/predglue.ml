module D = Predicate
module Pa = Path

module F = Ast

let str_to_path = Hashtbl.create 37
let path_to_str = Hashtbl.create 37

let str_of_path p =
  let _ = if Pa.unique_name p = "e0_736" then assert false else () in
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

let f_of_dbrel = function
  | D.Eq -> F.Eq
  | D.Ne -> F.Ne
  | D.Gt -> F.Gt
  | D.Ge -> F.Ge
  | D.Lt -> F.Lt
  | D.Le -> F.Le

let rec fsort_of_dprover_t = function
  | Parsetree.Pprover_array (t1, t2) -> F.Sort.Array (fsort_of_dprover_t t1, fsort_of_dprover_t t2)
  | Parsetree.Pprover_fun ts -> F.Sort.Func (List.map fsort_of_dprover_t ts)
  | Parsetree.Pprover_abs ("int") -> F.Sort.Int 
  | Parsetree.Pprover_abs ("bool") -> F.Sort.Bool
  | Parsetree.Pprover_abs s -> F.Sort.Unint s

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

