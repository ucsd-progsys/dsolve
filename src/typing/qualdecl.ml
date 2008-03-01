open Parsetree
open Predicate


module C = Common
module T = Types
module QG = Qualgen
module AM = Map.Make(String)
module TS = QG.TS
module IS = QG.IS

let fa env m (id, ty) =
  let s = if AM.mem id m then AM.find id m else TS.empty in
  AM.add id (TS.add (Typetexp.transl_type_scheme env ty) s) m 

let lst s k = s::k

let conflat y = String.concat "." (Longident.flatten y)

let rel_star = [Ge; Le; Ne]
let op_star = [Plus; Minus; Times; Div]
let transl_ops ops = 
  match ops with 
      [] -> op_star 
    | _ -> List.map transl_op ops
let transl_rels rels = 
  match rels with 
      [] -> rel_star 
    | _ -> List.map transl_rel rels

let transl_patpred_single p =
  let rec transl_expr_rec pe =
    match pe.ppredpatexp_desc with
      | Ppredpatexp_int (n) ->
	        PInt (n)
      | Ppredpatexp_var (y) -> (* flatten longidents for now -- need to look these up? *)
	        Var (Path.mk_ident (conflat y))
      | Ppredpatexp_funapp (f, es) ->
	        FunApp (conflat f, List.map transl_expr_rec es)
      | Ppredpatexp_binop (e1, ops, e2) ->
	        Binop (transl_expr_rec e1, transl_op (List.hd ops), transl_expr_rec e2)
      | Ppredpatexp_field (f, e1) ->
          Field (f, transl_expr_rec e1)
      | _ -> assert false
  in
  let rec transl_pred_rec pd =
    match pd.ppredpat_desc with
      | Ppredpat_true -> 
          True
      | Ppredpat_atom (e1, rels, e2) ->
	        Atom (transl_expr_rec e1, transl_rel (List.hd rels), transl_expr_rec e2)
      | Ppredpat_not (p) -> 
          Not (transl_pred_rec p)
      | Ppredpat_and (p1, p2) -> 
          And (transl_pred_rec p1, transl_pred_rec p2)
      | Ppredpat_or (p1, p2) -> 
          Or (transl_pred_rec p1, transl_pred_rec p2)
  in transl_pred_rec p

             
let transl_patpred tymap p =
  let rec transl_expr_rec pe =
    match pe.ppredpatexp_desc with
      | Ppredpatexp_int (n) ->
	        PPInt ([n])
      | Ppredpatexp_any_int ->
          PPInt (QG.all_consts ())      
      | Ppredpatexp_var (y) -> (* flatten longidents for now -- need to look these up? *)
	        PVar ([Path.mk_ident (conflat y)])
      | Ppredpatexp_mvar (y) ->
          PVar (if AM.mem y tymap then 
                List.map Path.mk_ident (IS.elements (List.fold_left IS.union IS.empty (List.map QG.findm (TS.elements (AM.find y tymap)))))
                else List.map Path.mk_ident (QG.all_ids ()))
      | Ppredpatexp_funapp (f, es) ->
	        PFunApp (f, List.map transl_expr_rec es)
      | Ppredpatexp_binop (e1, ops, e2) ->
	        PBinop (transl_expr_rec e1, transl_ops ops, transl_expr_rec e2)
      | Ppredpatexp_field (f, e1) ->
          PField (f, transl_expr_rec e1)
  in
  let rec transl_pred_rec pd =
    match pd.ppredpat_desc with
      | Ppredpat_true -> 
          PTrue
      | Ppredpat_atom (e1, rels, e2) ->
	        PAtom (transl_expr_rec e1, transl_rels rels, transl_expr_rec e2)
      | Ppredpat_not (p) -> 
          PNot (transl_pred_rec p)
      | Ppredpat_and (p1, p2) -> 
          PAnd (transl_pred_rec p1, transl_pred_rec p2)
      | Ppredpat_or (p1, p2) -> 
          POr (transl_pred_rec p1, transl_pred_rec p2)
  in transl_pred_rec p

let rec lflap es =
  match es with
    | s :: [] ->
        List.map (fun c -> [c]) s
    | s :: es ->
        C.flap (fun c -> List.map (fun d -> c :: d) (lflap es)) s
    | [] ->
        []

let tflap3 (e1, e2, e3) f =
  C.flap (fun c -> C.flap (fun d -> List.map (fun e -> f c d e) e3) e2) e1

let tflap2 (e1, e2) f =
  C.flap (fun c -> List.map (fun d -> f c d) e2) e1

let gen_preds p =
  let rec gen_expr_rec pe =
    match pe with
      | PPInt (ns) ->
          List.map (fun c -> PInt (c)) ns  
      | PVar (ps) ->
          List.map (fun c -> Var (c)) ps
      | PFunApp (f, es) ->
          let f' = conflat f in
          let ess = List.map gen_expr_rec es in
            List.map (fun e -> FunApp (f', e)) (lflap ess) 
      | PBinop (e1, ops, e2) ->
          let e1s = gen_expr_rec e1 in
          let e2s = gen_expr_rec e2 in
            tflap3 (e1s, ops, e2s) (fun c d e -> Binop (c, d, e))
      | PField (f, e1) ->
          let e1s = gen_expr_rec e1 in
            List.map (fun e -> Field(f, e)) e1s
  in    
  let rec gen_pred_rec pd =
    match pd with
      | PTrue ->
          [True] 
      | PNot (p) ->  
          List.map (fun c -> Not (c)) (gen_pred_rec p) 
      | POr (p1, p2) ->
          let p1s = gen_pred_rec p1 in
          let p2s = gen_pred_rec p2 in
            tflap2 (p1s, p2s) (fun c d -> Or (c, d))
      | PAnd (p1, p2) ->
          let p1s = gen_pred_rec p1 in
          let p2s = gen_pred_rec p2 in
            tflap2 (p1s, p2s) (fun c d -> And (c, d))
      | PAtom (e1, rels, e2) ->      
          let e1s = gen_expr_rec e1 in
          let e2s = gen_expr_rec e2 in
            tflap3 (e1s, rels, e2s) (fun c d e -> Atom (c, d, e))
      | PIff (e1, p1) ->
          let e1s = gen_expr_rec e1 in
          let p1s = gen_pred_rec p1 in
            tflap2 (e1s, p1s) (fun c d -> Iff (c, d))
  in gen_pred_rec p

(* Translate a qualifier declaration *)
let transl_pattern env {Parsetree.pqual_pat_desc = (valu, anno, pred)} =
  let preds = (gen_preds (transl_patpred (List.fold_left (fa env) AM.empty anno) pred)) in
  preds

let transl_pattern_valu env ({Parsetree.pqual_pat_desc = (valu, anno, pred)} as p) =
  let preds = transl_pattern env p in
    List.map (fun p -> (valu, p)) preds
