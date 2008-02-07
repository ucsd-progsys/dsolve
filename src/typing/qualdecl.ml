open Parsetree
open Predicate

module QG = Qualgen
module AM = Map.Make(String)
module IS = Set.Make(struct
                      type t = Parsetree.core_type
                      let compare = compare
                     end)

let fa m (id, ty) =
  let s = if AM.mem id m then AM.find id m else IS.empty in
  AM.add id (IS.add ty s) m 
   
let lst s k = s::k

let conflat y = String.concat "." (Longident.flatten y)



let fold_patpred_lists g p =
  let rec fold_expr_rec pe =
    match pe with
      | PPInt (ns) ->
          List.length ns 
      | PVar (ps) ->
          List.length ps
      | PFunApp (f, es) ->
          g (List.map fold_expr_rec es) 
      | PBinop (e1, ops, e2) ->
          g [(List.length ops); fold_expr_rec e1; fold_expr_rec e2] 
  in    
  let rec fold_pred_rec pd =
    match pd with
      | PTrue -> 
          g [] 
      | PNot (p) ->  
          fold_pred_rec p 
      | POr (p1, p2)
      | PAnd (p1, p2) ->
          g [fold_pred_rec p1; fold_pred_rec p2]
      | PAtom (p1, rels, p2) ->      
          g [(List.length rels); fold_expr_rec p1; fold_expr_rec p2]
      | PIff (p1, p2) ->
          g [fold_expr_rec p1; fold_pred_rec p2]
  in fold_pred_rec p

(*let fold_patpred_lists f g p =
  let rec fold_expr_rec pe =
    match pe with
      | PPInt (ns) ->
          f ns 
      | PVar (ps) ->
          f ps
      | PFunApp (f, es) ->
          g (List.map fold_expr_rec es) 
      | PBinop (e1, ops, e2) ->
          g [(f ops); fold_expr_rec e1; fold_expr_rec e2] 
  in    
  let rec fold_pred_rec pd =
    match pd with
      | PTrue -> 
          g [] 
      | PNot (p) ->  
          fold_pred_rec p 
      | POr (p1, p2)
      | PAnd (p1, p2) ->
          g [fold_pred_rec p1; fold_pred_rec p2]
      | PAtom (p1, rels, p2) ->      
          g [(f rels); fold_pred_rec p1; fold_pred_rec p2]
  in fold_pred_rec p*)


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
                  List.flatten (List.map QG.lookup_ids (IS.elements (AM.find y tymap))) 
                else QG.all_ids)
      | Ppredpatexp_funapp (f, es) ->
	        PFunApp (f, List.map transl_expr_rec es)
      | Ppredpatexp_binop (e1, ops, e2) ->
	        PBinop (transl_expr_rec e1, transl_ops ops, transl_expr_rec e2)
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




(* relies on deterministic traversal order -- should be OK *)
(*let gen_patpred_digits b p =
  let b = ref b in
  let hdb () = let bb = List.hd !b in (b := List.tl !b; bb) in
  let rec gen_expr_rec pe =
    match pe with
      | PPInt (ns) ->
          PInt (List.nth (hdb ()) ns)  
      | PVar (ps) ->
          Var (List.nth (hdb ()) ps) 
      | PFunApp (f, es) ->
          FunApp (f, List.map gen_expr_rec es)  
      | PBinop (e1, ops, e2) ->
          BinOp (gen_expr_rec e1, List.nth (hdb ()) ops, gen_expr_rec e2)  
  in    
  let rec gen_pred_rec pd =
    match pd.ppredpat_desc with
      | PTrue 
          True 
      | PNot (p) ->  
          Not (gen_pred_rec p) 
      | POr (p1, p2) ->
          Or (gen_pred_rec p1, gen_pred_rec p2)
      | PAnd (p1, p2) ->
          And (gen_pred_rec p1, gen_pred_rec p2)
      | PAtom (p1, rels, p2) ->      
          Atom (gen_pred_rec p1, List.nth (hdb ()) rels, gen_pred_rec p2)
  in gen_pred_rec p

let size_pred = fold_patpred_lists (List.fold_left ( * ) 1)

let list_pred = fold_patpred_lists (List.flatten)

let build_nth_pred n pat = 
  let base = ref (list_pred pat) in
  let digits = ref (QG.decode (n, !base)) in
    gen_patpred_digits digits pat*) 

let gen_predicates v a = 
  [(Ident.create v, True)]

(* Translate a qualifier declaration *)
let transl_pattern {Parsetree.pqual_pat_desc = (valu, anno, pred)} =
    (gen_predicates valu (transl_patpred (List.fold_left fa AM.empty anno) pred))


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
