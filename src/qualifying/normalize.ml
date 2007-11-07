open Parsetree
open Asttypes
open Format


let wrap_printable exp = (Ptop_def([{pstr_desc = (Pstr_eval exp); pstr_loc = Location.none}])) 
    
    (* ming: I think it's actually better to pass everything around as
     * longidents for true generality, but applies of functors should never
     * happen in our code so this assumption is OK *)
let li_flatten li = String.concat "." (Longident.flatten li) 

(* constant divisions and multiplications are wreaking havoc on us
 * because non-const ops turn into uninterpreted functions and make
 * life difficult *)
let is_op exp nm = 
  match exp.pexp_desc with
    | Pexp_ident(id) ->
        li_flatten id = nm
    | _ -> false

let is_mult exp = 
  is_op exp "*"
let is_div exp =
  is_op exp "/"
               
let is_const exp =
  match exp.pexp_desc with
    | Pexp_constant(Const_int _) ->
        true
    | _ -> false

let is_const_div exp = 
  match exp.pexp_desc with 
    Pexp_apply(e1, es) ->
      let es = List.map (fun (_, e) -> e) es in
      let div = is_div e1 in
        if div then is_const (List.nth es 1) else false 
    | _ -> false
      
let is_const_mult exp =
  match exp.pexp_desc with
      Pexp_apply(e1, es) ->
        let es = List.map (fun (_, e) -> e) es in
        let mult = is_mult e1 in
          if mult then is_const (List.nth es 1) || is_const (List.hd es)
          else false
    | _ -> false

let normalize exp =
  let next_name_cnt = ref 0 in
  let fresh_name () =
    let i = !next_name_cnt in
    let _ = next_name_cnt := !next_name_cnt + 1 in
      ("__tmp"^(string_of_int i))  
  in

  (* ming: we dummy out all the pattern locations because we don't use them.
   * this is technically destructive though, if we do implement pattern matching
   * it will have to be fixed. *)
  let mk_let r x e1 e2 = Pexp_let(r, [({ppat_desc = Ppat_var x; ppat_loc = Location.none}, e1)], e2) in
  let mk_apply e1 es = Pexp_apply(e1, (List.map (fun e -> ("", e)) es)) in
  let mk_ident id = Pexp_ident(id) in
  let mk_function lbl elbl arg_pat sube = Pexp_function(lbl, elbl, [(arg_pat, sube)]) in
  let mk_array es = Pexp_array(es) in
  let mk_tuple es = Pexp_tuple(es) in
  let mk_sequence e1 e2 = Pexp_sequence(e1, e2) in
  let mk_ifthenelse e1 e2 e3 = Pexp_ifthenelse(e1, e2, Some e3) in

  let mk_dummy desc = {pexp_desc = desc; pexp_loc = Location.none} in

  (*let mk_dum_let r x e1 e2 = mk_dummy (mk_let r x e1 e2) in
  let mk_dum_apply e1 es = mk_dummy (mk_apply e1 es) in*)
  let mk_dum_ident id = mk_dummy (mk_ident id) in
  (*let mk_dum_function lbl elbl arg_pat sube = mk_dummy (mk_function lbl elbl arg_pat sube) in
  let mk_dum_array es = mk_dummy (mk_array es) in
  let mk_dum_tuple es = mk_dummy (mk_tuple es) in
  let mk_dum_sequence e1 e2 = mk_dummy (mk_sequence e1 e2) in*)
  let mk_dum_ifthenelse e1 e2 e3 = mk_dummy (mk_ifthenelse e1 e2 e3) in
  
  (*let fresh_ident () = mk_dum_ident (Longident.parse (fresh_name ())) in*)
  let mk_dum_ident_lbl lbl = mk_dum_ident (Longident.parse lbl) in
  let mk_list_let r ls e2 = 
    let ls = List.map (fun (x, e) -> 
                         let e = match e with Some e -> e
                           | None -> mk_dum_ident_lbl x
                         in ({ppat_desc = Ppat_var x; ppat_loc = Location.none}, e)) ls in
      Pexp_let(r, ls, e2) in

  let rec norm_bind_list bs = 
    let single_bind elt =
      match elt with
        ({ppat_desc = Ppat_var x}, e1) ->
            let ls = norm_in e1 in
            let (_, e_f) = List.hd ls in
              ((x, e_f), List.tl ls) 
        | ({ppat_desc = Ppat_any}, e1) -> 
            let ls = norm_in e1 in
              (List.hd ls, List.tl ls)
        | _ -> assert false
    in
    let prs = (List.map single_bind bs) in
    let k (q, r) (qs, rs) = (q::qs, r::rs) in
    let (ins, outs) = List.fold_right k prs ([], [])  in
      (ins, List.concat outs)
  and norm_out exp =
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let wrap r b (lbl, a) = 
      match a with
          Some a -> mk_let r lbl a (mk_dummy b)
          | None -> b
    in
    let proc_list es skel =
     let lss = List.map norm_in es in
     let lbls = List.map (fun ls -> let (lbl, e_l) = List.hd ls in (mk_dum_ident_lbl lbl)) lss in
     let init = skel lbls in
      rw_expr (List.fold_left (wrap Nonrecursive) init (List.concat (List.rev lss)))
    in

    match exp.pexp_desc with
     | Pexp_constant(_) 
     | Pexp_construct(_, None, false) ->
        let a = fresh_name () in
         rw_expr (mk_let Nonrecursive a exp (mk_dum_ident_lbl a))
     | Pexp_ident(_) ->
        exp
     | Pexp_function(lbl, elbl, [(arg, e)]) ->
        rw_expr (mk_function lbl elbl arg (norm_out e))
     | Pexp_let(r, [({ppat_desc = Ppat_any}, e1)], e2) ->
        norm_out (rw_expr (mk_let r (fresh_name ()) e1 e2))
     | Pexp_let(r, [({ppat_desc = Ppat_var x}, e1)], e2) -> 
        let ls = norm_in e1 in 
        (* ming: e2 is already written with lbl x, so we have to ignore the lbl
         * generated by norm_in and wrap the first layer ourselves *)
        let (lbl, e_1) = List.hd ls in
        let init = 
          match e_1 with
              Some e -> mk_let r x e (norm_out e2) 
              | None -> mk_let r x (mk_dum_ident_lbl lbl) (norm_out e2)
        in
         rw_expr (List.fold_left (wrap r) init (List.tl ls))
     (* to be particularly careful, we'll add a special case for mutual
      * binds (keyword and) *)
     | Pexp_let(r, pes, e2) ->
        let (ins, outs) = norm_bind_list pes in        
        let init = norm_out e2 in 
        let init = mk_list_let r ins init in
          rw_expr (List.fold_left (wrap r) init outs)
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, _) = List.hd f in 
        let es = List.map (fun (_, e) -> e) es in
        let lss = List.map norm_in es in
        let ts = List.map (fun ls -> let (lbl, _) = List.hd ls in mk_dum_ident_lbl lbl) lss in
          (* ming: hack for constant div *)
        let ts = if is_const_div exp || is_const_mult exp then
                    let e_n1 = List.hd ts in
                    let e_n2 = List.nth ts 1 in
                    let e_o1 = List.hd es in
                    let e_o2 = List.nth es 1 in
               [if is_const e_o1 then e_o1 else e_n1;
                if is_const e_o2 then e_o2 else e_n2]
               else ts in
        let init = mk_apply (mk_dum_ident_lbl flbl) ts in
        let ls = List.concat (List.rev (f::lss)) in
         rw_expr (List.fold_left (wrap Nonrecursive) init ls)  
     | Pexp_ifthenelse(e1, e2, Some e3) ->
         (* toss out the first label, since we know that hte first expression
          * will be normalized, just use that as the conditional *)
        let b = norm_in e1 in
        let (blbl, e_b) = List.hd b in
        let init = match e_b with Some e_b -> mk_ifthenelse e_b (norm_out e2) (norm_out e3)
                    | None -> mk_ifthenelse (mk_dum_ident_lbl blbl) (norm_out e2) (norm_out e3) in
         rw_expr (List.fold_left (wrap Nonrecursive) init (List.tl b))
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array
     | Pexp_sequence(e1, e2) ->
        rw_expr (mk_sequence (norm_out e1) (norm_out e2))
     | Pexp_assertfalse ->
        exp
     | e -> printf "@[Bad expr to norm_out:@\n%a@]" Printast.top_phrase (wrap_printable exp); assert false

  and norm_in exp = 
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let proc_list es skel = 
      let this = fresh_name () in
      let lss = List.map norm_in es in
      let lbls = List.map (fun ls -> let (lbl, _) = List.hd ls in (mk_dum_ident_lbl lbl)) lss in
      let e_this = rw_expr (skel lbls) in
        (this, Some e_this)::(List.concat (List.rev lss))
    in

    match exp.pexp_desc with
     | Pexp_assertfalse ->
         [(fresh_name (), Some (norm_out exp))]
     | Pexp_constant(_)      
     | Pexp_construct(_, None, false) ->
         [(fresh_name (), Some exp)]
     | Pexp_ident(id) ->
         (* ming: to preserve annotations over idents we actually want something
          * like (id, exp), but that will add another layer to all scopes and
          * generally make life more difficult while debugging. will turn this
          * off eventually *)
         [((li_flatten id), None)]
     | Pexp_sequence(_, _) 
     | Pexp_function(_, _, _)     
     | Pexp_let(_, _, _) -> 
        [(fresh_name (), Some (norm_out exp))]
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array 
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, e_f) = List.hd f in
        let es = List.map (fun (_, e) -> e) es in
        let ls = proc_list es (mk_apply (mk_dum_ident_lbl flbl)) in
        let (this, e_this) = List.hd ls in
          (* ming: hack for constant div *)
        let e_this =  
          if is_const_div exp || is_const_mult exp then 
            begin
            match e_this with
                Some e_this ->
                    begin match e_this.pexp_desc with
                    | Pexp_apply(e1, es') ->
                      let e_n1 = List.hd es' in
                      let e_n2 = List.nth es' 1 in
                      let e_o1 = List.hd es in
                      let e_o2 = List.nth es 1 in
                      let fst = if is_const e_o1 then ("", e_o1) else e_n1 in
                      let snd = if is_const e_o2 then ("", e_o2) else e_n2 in
                      let es' = [fst; snd] in
                      Some {pexp_desc = Pexp_apply(e1, es'); 
                            pexp_loc = e_this.pexp_loc}
                    | _ -> assert false
                    end
                | None -> assert false
            end
          else e_this in
          (this, e_this)::(List.append f (List.tl ls))
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        (* toss out the first label, since we know that expression will be
         * normalized *)
        let b = norm_in e1 in
        let (blbl, e_b) = List.hd b in
        let (this, e_this) = (fresh_name (), 
                              match e_b with Some e_b -> mk_dum_ifthenelse e_b (norm_out e2) (norm_out e3)
                                | None -> mk_dum_ifthenelse (mk_dum_ident_lbl blbl) (norm_out e1) (norm_out e3)) in
         (this, Some e_this)::(List.tl b)
     | e -> printf "@[Bad expr to norm_in:@\n%a@]" Printast.top_phrase (wrap_printable exp); assert false
  in
  norm_out exp



let rec normalize_structure sstr =
  match sstr with
    [] -> []
    | {pstr_desc = (Pstr_eval exp); pstr_loc = loc} :: srem ->
        let normal_exp = normalize exp in
        let _ = printf "@[%a@\n@]" Qdebug.pprint_expression normal_exp in
        ({pstr_desc = (Pstr_eval(normal_exp)) ; pstr_loc = loc}) :: (normalize_structure srem)
    | p :: srem -> p :: (normalize_structure srem)
