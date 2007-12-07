open Parsetree
open Asttypes
open Format

  (* ming: TODO -- we cna just add a location to subexpr pairs that are passed
   * up (making them triples) to keep ident lcoations *)


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

let is_function exp =
  match exp.pexp_desc with
    | Pexp_function(_, _, _) ->
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
      Longident.parse ("__tmp"^(string_of_int i))  
  in

  (* ming: we dummy out all the pattern locations because we don't use them.
   * this is technically destructive though, if we do implement pattern matching
   * it will have to be fixed. *)
  let mk_let r x e1 e2 = Pexp_let(r, [({ppat_desc = Ppat_var x; ppat_loc = Location.none}, e1)], e2) in
  let mk_let_lbl r x e1 e2 = mk_let r (li_flatten x) e1 e2 in
  let mk_let_gen r pat e1 e2 = Pexp_let(r, [(pat, e1)], e2) in
  let mk_apply e1 es = Pexp_apply(e1, (List.map (fun e -> ("", e)) es)) in
  let mk_ident id = Pexp_ident(id) in
  let mk_function lbl elbl arg_pat sube = Pexp_function(lbl, elbl, [(arg_pat, sube)]) in
  let mk_array es = Pexp_array(es) in
  let mk_tuple es = Pexp_tuple(es) in
  let mk_sequence e1 e2 = Pexp_sequence(e1, e2) in
  let mk_ifthenelse e1 e2 e3 = Pexp_ifthenelse(e1, e2, Some e3) in
  let mk_field e s = Pexp_field(e, s) in
  let mk_record es = Pexp_record(es, None) in

  let mk_dummy desc = {pexp_desc = desc; pexp_loc = Location.none} in
  let dummy = Location.none in

  (*let mk_dum_let r x e1 e2 = mk_dummy (mk_let r x e1 e2) in
  let mk_dum_apply e1 es = mk_dummy (mk_apply e1 es) in*)
  let mk_dum_ident id = mk_dummy (mk_ident id) in
  let mk_ident_loc id loc = {pexp_desc = mk_ident id; pexp_loc = loc} in
  (*let mk_dum_function lbl elbl arg_pat sube = mk_dummy (mk_function lbl elbl arg_pat sube) in
  let mk_dum_array es = mk_dummy (mk_array es) in
  let mk_dum_tuple es = mk_dummy (mk_tuple es) in
  let mk_dum_sequence e1 e2 = mk_dummy (mk_sequence e1 e2) in
  let mk_dum_ifthenelse e1 e2 e3 = mk_dummy (mk_ifthenelse e1 e2 e3) in*)
  
  (*let fresh_ident () = mk_dum_ident (Longident.parse (fresh_name ())) in*)
  let mk_list_let r ls e2 = 
    let ls = List.map (fun (x, e, lo) -> 
                         let e = match e with Some e -> e
                           | None -> mk_ident_loc x lo
                         in ({ppat_desc = Ppat_var (li_flatten x); ppat_loc = Location.none}, e)) ls in
      Pexp_let(r, ls, e2) in

    (* still broken for general patterns; and just in general *)
  let rec norm_bind_list bs = 
    let single_bind elt =
      match elt with
        ({ppat_desc = Ppat_var x}, e1) ->
            let ls = norm_in e1 in
            let (lbl, e_f, lo) = List.hd ls in
              begin
              match e_f with 
                  Some e -> 
                    ((Longident.parse x, e_f, lo), List.tl ls) 
                | None ->
                    ((Longident.parse x, Some (mk_ident_loc lbl lo), lo), List.tl ls)  
              end
        | ({ppat_desc = Ppat_any}, e1) -> 
            let ls = norm_in e1 in
            let (this, e_this, loc_this) = List.hd ls in
            let this = 
              match e_this with
                  Some e -> (this, e_this, loc_this)
                | None -> (this, Some (mk_ident_loc this loc_this), loc_this) 
            in
              (this, List.tl ls)
        | _ -> assert false
    in
    let prs = (List.map single_bind bs) in
    let k (q, r) (qs, rs) = (q::qs, r::rs) in
    let (ins, outs) = List.fold_right k prs ([], [])  in
    (*let _ = if List.length outs > 0 && List.length ins > 1 then assert false
     * else () in*)
      (ins, List.concat outs)
  and norm_out exp =
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let wrap r b (lbl, a, _) = 
      match a with
          Some a -> mk_let_lbl r lbl a (mk_dummy b)
          | None -> b
    in
    let proc_list es skel =
     let lss = List.map norm_in es in
     let lbls = List.map (fun ls -> let (lbl, e, lo) = List.hd ls in 
                                        match e with
                                            Some e -> if is_const e then e else mk_ident_loc lbl lo
                                          | None -> mk_ident_loc lbl lo) lss in
     let init = skel lbls in
      rw_expr (List.fold_left (wrap Nonrecursive) init (List.concat (List.rev lss)))
    in

    match exp.pexp_desc with
     | Pexp_constant(_) 
     | Pexp_construct(_, None, false) ->
        let a = fresh_name () in
         rw_expr (mk_let_lbl Nonrecursive a exp (mk_dum_ident a))
     | Pexp_constraint(_, _, _)
     | Pexp_ident(_) ->
        exp
     | Pexp_function(lbl, elbl, [(arg, e)]) ->
        rw_expr (mk_function lbl elbl arg (norm_out e))
     | Pexp_let(r, [(pat, e1)], e2) ->
        let ls = norm_in e1 in
        let (lbl, e_1, lo) = List.hd ls in
        let init =
          match e_1 with
              Some e -> mk_let_gen r pat e (norm_out e2)
            | None -> mk_let_gen r pat (mk_ident_loc lbl lo) (norm_out e2)
        in
          rw_expr (List.fold_left (wrap r) init (List.tl ls))
      (* this is generally broken, but OK for actual mutual recursion *)
     | Pexp_let(r, pes, e2) ->
        let (ins, outs) = norm_bind_list pes in        
        let init = norm_out e2 in 
        let init = mk_list_let r ins init in
          rw_expr (List.fold_left (wrap r) init outs)
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, _, lo) = List.hd f in 
        let es = List.map (fun (_, e) -> e) es in
        let lss = List.map norm_in es in
        let ts = List.map (fun ls -> let (lbl, _, lo) = List.hd ls in mk_ident_loc lbl lo) lss in
          (* ming: hack for constant div *)
        let ts = if is_const_div exp || is_const_mult exp then
                    let e_n1 = List.hd ts in
                    let e_n2 = List.nth ts 1 in
                    let e_o1 = List.hd es in
                    let e_o2 = List.nth es 1 in
               [if is_const e_o1 then e_o1 else e_n1;
                if is_const e_o2 then e_o2 else e_n2]
               else ts in
        let init = mk_apply (mk_ident_loc flbl lo) ts in
        let ls = List.concat (List.rev (f::lss)) in
         rw_expr (List.fold_left (wrap Nonrecursive) init ls)  
     | Pexp_ifthenelse(e1, e2, Some e3) ->
         (* toss out the first label, since we know that hte first expression
          * will be normalized, just use that as the conditional *)
        let b = norm_in e1 in
        let (blbl, e_b, lo) = List.hd b in
        let init = match e_b with Some e_b -> mk_ifthenelse e_b (norm_out e2) (norm_out e3)
                    | None -> mk_ifthenelse (mk_ident_loc blbl lo) (norm_out e2) (norm_out e3) in
         rw_expr (List.fold_left (wrap Nonrecursive) init (List.tl b))
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array
          (* lose location for the whole sequence, but that's generally ok since
           * we don't lose location of second expression *)
     | Pexp_sequence(e1, e2) ->
        rw_expr (mk_sequence (norm_out e1) (norm_out e2))
     | Pexp_assertfalse ->
        exp
     | Pexp_field(e, s) ->
        let ls = norm_in e in
        let (lbl, _, lo) = List.hd ls in 
        let init = mk_field (mk_ident_loc lbl lo) s in
          rw_expr (List.fold_left (wrap Nonrecursive) init ls)
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in 
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let init = mk_record es' in
        let ee = List.concat (List.rev ee) in
          rw_expr (List.fold_left (wrap Nonrecursive) init ee)
     | e -> printf "@[Bad expr to norm_out:@\n%a@]" Qdebug.pprint_expression exp; flush stdout; assert false

  and norm_in exp = 
    let rw_expr desc = {pexp_desc = desc; pexp_loc = exp.pexp_loc} in
    let proc_list es skel = 
      let this = fresh_name () in
      let lss = List.map norm_in es in
      let lbls = List.map (fun ls -> let (lbl, e, lo) = List.hd ls in 
                                        match e with
                                            Some e -> if is_const e then e else mk_ident_loc lbl lo
                                          | None -> mk_ident_loc lbl lo) lss in
      let e_this = rw_expr (skel lbls) in
        (this, Some e_this, dummy)::(List.concat (List.rev lss))
    in
    let loc = exp.pexp_loc in

    match exp.pexp_desc with
     | Pexp_assertfalse ->
         [(fresh_name (), Some (norm_out exp), dummy)]
     | Pexp_constraint(_, _, _)
     | Pexp_constant(_)      
     | Pexp_construct(_, None, false) ->
         [(fresh_name (), Some exp, dummy)]
     | Pexp_ident(id) ->
         (* ming: to preserve annotations over idents we actually want something
          * like (id, exp), but that will add another layer to all scopes and
          * generally make life more difficult while debugging. will turn this
          * off eventually *)
         [(id, None, loc)]
     | Pexp_function(_, _, _)     
     | Pexp_let(_, _, _) -> 
        [(fresh_name (), Some (norm_out exp), dummy)]
          (* ming: pull sequences out to the closest scope *)
     | Pexp_sequence(e1,e2) ->
        let ls1 = norm_in e1 in
        let ls2 = norm_in e2 in
          List.append ls2 ls1
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
         (* tmp hack for huge constant arrays *)
        proc_list es mk_array 
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, e_f, lo_f) = List.hd f in
        let es = List.map (fun (_, e) -> e) es in
        let fn = match e_f with Some e -> mk_dum_ident flbl
                                | None -> mk_ident_loc flbl lo_f
        in
        let ls = proc_list es (mk_apply fn) in
        let (this, e_this, lo_this) = List.hd ls in
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
          (this, e_this, lo_this)::(List.append (List.tl ls) f)
     | Pexp_ifthenelse(e1, e2, Some e3) ->
        (* toss out the first label, since we know that expression will be
         * normalized *)
        let b = norm_in e1 in
        let (blbl, e_b, lo_b) = List.hd b in
        let (this, e_this, lo_this) = (fresh_name (), 
                              (match e_b with Some e_b -> mk_ifthenelse e_b (norm_out e2) (norm_out e3)
                                | None -> mk_ifthenelse (mk_ident_loc blbl lo_b) (norm_out e2) (norm_out e3)),
                                       dummy) in
         (this, Some (rw_expr e_this), lo_this)::(List.tl b)
     | Pexp_record(es, None) ->
        let ee = List.map (fun (s, e) -> norm_in e) es in
        let se = List.map (fun e -> List.hd e) ee in
        let es' = List.map2 (fun (lbl, _, loc) (s, e) -> (s, mk_ident_loc lbl loc)) se es in
        let e_this = rw_expr (mk_record es') in
          (fresh_name (), Some e_this, loc)::(List.concat (List.rev ee)) 
     | Pexp_field(e, s) ->
        let ls = norm_in e in 
        let (lbl, _, lo) = List.hd ls in
          (fresh_name (), Some (rw_expr (mk_field (mk_ident_loc lbl lo) s)), loc)::ls
     | e -> printf "@[Bad expr to norm_in:@\n%a@]" Printast.top_phrase (wrap_printable exp); assert false
  in
  norm_out exp



let rec normalize_structure sstr =
  let _ = Format.set_margin 170 in
  let _ = Format.set_max_indent 150 in
  match sstr with
    [] -> []
    | {pstr_desc = (Pstr_eval exp); pstr_loc = loc} :: srem ->
        let _ = printf "@[Pstr_eval@\n@]" in
        let normal_exp = normalize exp in
        let _ = printf "@[%a@\n@]" Qdebug.pprint_expression normal_exp in
        ({pstr_desc = (Pstr_eval(normal_exp)) ; pstr_loc = loc}) :: (normalize_structure srem)
    | {pstr_desc = (Pstr_value(recursive, pl)); pstr_loc = loc} :: srem -> 
        (* assume with all accompanying losses of generality that no one is
         * stupid enough to use and without rec *)
        let _ = printf "@[Pstr_value@\n@]" in
        let value = {pstr_desc = (Pstr_value(recursive, List.map (fun (p, exp) -> (p, normalize exp)) pl)); pstr_loc = loc} in
        let _ = printf "@[%a@\n@]" Qdebug.pprint_structure value in
        value :: (normalize_structure srem)
    | p :: srem -> 
        let _ = printf "@[did nothing@\n@]" in
        p :: (normalize_structure srem) 
