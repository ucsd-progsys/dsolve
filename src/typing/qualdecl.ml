open Parsetree
open Predicate


module C = Common
module T = Types
module QG = Qualgen
module AM = Map.Make(String)
module TS = Set.Make(struct
                      type t = T.type_expr 
                      let compare = compare
                     end)

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
             
let transl_patpred tymap p =
  let rec transl_expr_rec pe =
    match pe.ppredpatexp_desc with
      | Ppredpatexp_int (n) ->
	        PPInt ([n])
      | Ppredpatexp_any_int ->
          PPInt (QG.all_consts)      
      | Ppredpatexp_var (y) -> (* flatten longidents for now -- need to look these up? *)
	        PVar ([Path.mk_ident (conflat y)])
      | Ppredpatexp_mvar (y) ->
          PVar (if AM.mem y tymap then 
                  List.flatten (List.map QG.lookup_ids (TS.elements (AM.find y tymap))) 
                else QG.all_ids)
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

let gen_quals nm ppat =
  let num = ref 0 in
  let n () = incr num; string_of_int !num in
  List.map (fun c -> (Ident.create (nm ^ (n ())) , c)) (gen_preds ppat)

(* Translate a qualifier declaration *)
let transl_pattern env {Parsetree.pqual_pat_desc = (valu, anno, pred)} =
    (gen_quals valu (transl_patpred (List.fold_left (fa env) AM.empty anno) pred))



(*let build_preds p =
  let max = ref (size_pred pat) in*)
  


(*let fold_patpred_lists f g p =
  let fa a f = f a in
  let rec fold_expr_rec pe =
    match pe.ppredpatexp_desc with
      | (fs, PPInt (ns)) ->
          List.forall (fun n -> preds := (List.fold_left fa (PInt(n)) fs)::!preds) ns 
      | (fs, PVar (ds)) ->
          List.forall (fun d -> preds := (List.fold_left fa (PVar(s)) fs)::!preds) ds
      | PFunApp (f, es) ->
           (List.map fold_expr_rec es) 
      | PBinop (e1, ops, e2) ->
          g [(f ops); fold_expr_rec e1; fold_expr_rec e2] 
  in    
  let rec fold_pred_rec pd =
    match pd.ppredpat_desc with
      | PTrue 
          g [] 
      | PNot (p) ->  
          fold_pred_rec p 
      | POr (p1, p2) ->
      | PAnd (p1, p2) ->
          g [fold_pred_rec p1; fold_pred_rec p2]
      | PAtom (p1, rels, p2) ->      
          g [(f rels); fold_pred_rec p1; fold_pred_rec p2]
  in fold_pred_rec p*)
