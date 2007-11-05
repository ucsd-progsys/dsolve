
let normalize exp =
  let next_name_cnt = ref 0 in
  let fresh_name () =
    let i = !next_name_cnt in
    let _ = next_name_cnt := !next_name_cnt + 1 in
      Longident.Lident ("__tmp"^(string_of_int i))  
  in

  let fresh_ident () = mk_dum_ident (fresh_name ()) in

  let mk_let r x e1 e2 = Pexp_let(r, [({pat_desc = Tpar_var x}, e_1)], e2) in
  let mk_apply e1 es = Pexp_apply(e1, es) in
  let mk_ident id = Pexp_ident(id) in
  let mk_function lbl elbl arg_pat sube = Pexp_function(lbl, elbl, [(arg_pat, sube)]) in
  let mk_array es = Pexp_array(es) in
  let mk_sequence e1 e2 = Pexp_sequence(e1, e2) in

  let mk_dummy desc = {pexp_desc = desc; pexp_loc = Location.none} in

  let mk_dum_let r x e1 e2 = mk_dummy (mk_let r x e1 e2) in
  let mk_dum_apply e1 es = mk_dummy (mk_apply e1 es) in
  let mk_dum_ident id = mk_dummy (mk_ident is) in
  let mk_dum_function lbl elbl arg_pat sube = mk_dummy (mk_function lbl elbl arg_pat sube) in
  let mk_dum_array es = mk_dummy (mk_array es) in
  let mk_dum_sequence e1 ee2 = mk_dummy (mk_sequence e1 e2) in

  let rec norm_out exp =
    let loc = exp.pexp_loc in
    let rw_expr desc = {pexp_desc = desc; pexp_loca = exp.loc} in
    let wrap r (lbl, a) b = mk_let r lbl a (mk_dummy b) in

    match exp.pexp_desc with
     | Pexp_constant(Const_int n) ->
     | Pexp_construct(_, None, false) ->
       let a = fresh_name () in
        rw_expr (mk_let Nonrecursive a exp (mk_dum_ident a))
     | Pexp_ident(_) ->
        exp
     | Pexp_function(lbl, elbl, [(arg, e)]) ->
        rw_expr (mk_function lbl elbl [(arg, norm_out e)])
     | Pexp_let(r, [({pat_desc = Tpar_var x}, e1)], e2) -> 
       let ls = norm_in e1 in 
        rw_expr (List.fold_left (wrap r) (norm_out e2) ls)
     | Pexp_apply(e1, es) ->
       let f = norm_in e1 in
       let (flbl, e_f) = List.hd f in 
       let _ = assert (List.length f = 1) in
       let lss = List.map norm_in es in
       let ts = List.map (fun ls -> let (lbl, e_l) = List.hd ls in lbl) lss in
       let init = mk_let Nonrecursive flbl e_f (mk_dum_apply flbl ts) in
       let ls = List.concat (List.rev lss) in
         List.fold_left (wrap Nonrecursive) init ls  
     | Pexp_
     | Pexp_tuple(es) 
     | Pexp_array(es) ->
       let lss = List.map norm_in es in
       let lbls = List.map (fun ls -> let (lbl, e_l) = List.hd ls in lbl) lss in
       let init = mk_dum_array lbls in
        rw_expr (List.fold_left (wrap Nonrecursive) init ls
     | Pexp_sequence(e1, e2) ->
        rw_expr (mk_sequence (norm_out e1) (norm_out e2))
     | e -> assert false

  and norm_in exp = 
    let rw_expr desc = {pexp_desc = desc; pexp_loca = exp.loc} in
    let proc_list es skel = 
      let this = fresh_ident () in
      let lss = List.map norm_in es in
      let lbls = List.map (fun ls -> let (lbl, _) = List.hd ls in lbl) lss in
      let e_this = rw_expr (skel lbls) in
        (this, e_this)::(List.concat lss)
    in

    match exp.pexp_desc with
     | Pexp_constant(Const_int n)      
     | Pexp_construct(_, None, false) 
     | Pexp_ident(_) ->
         [(fresh_ident (), exp)]
     | Pexp_sequence(_, _) 
     | Pexp_function(_, Some arg, ls)     
     | Pexp_let(_, _, _) -> 
        [(fresh_ident (), norm_out exp)]
     | Pexp_tuple(es) ->
        proc_list es mk_tuple
     | Pexp_array(es) ->
        proc_list es mk_array 
     | Pexp_apply(e1, es) ->
        let f = norm_in e1 in
        let (flbl, e_f) = List.hd f in
        let _ = assert List.length f = 1 in
          (flbl, e_f)::(proc_list es (mk_apply flbl))
     | e -> assert false
in
norm_out exp



