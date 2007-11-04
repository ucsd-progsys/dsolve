
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

  let mk_dummy desc = {pexp_desc = desc; pexp_loc = Location.none} in

  let mk_dum_let r x e1 e2 = mk_dummy (mk_let r x e1 e2) in
  let mk_dum_apply e1 es = mk_dummy (mk_apply e1 es) in
  let mk_dum_ident id = mk_dummy (mk_ident is) in

  let rec norm_out exp =
    let loc = exp.pexp_loc in
    let rw_expr desc = {pexp_desc = desc; pexp_loca = loc} in

    match exp.pexp_desc with
    | Pexp_constant(Const_int n) ->
        let a = fresh_name () in
        rw_expr (mk_let Nonrecursive a exp (mk_dum_ident a))
    | Pexp_let(r, [({pat_desc = Tpar_var x}, e1)], e2) -> 
        let (ls, e_in) = norm_in e1 in 
        let init = mk_dum_let r x e_in (norm_out e2) in
        let compound b (lbl, a) = mk_let r lbl a (mk_dummy b) in
          rw_expr (List.fold_right compound ls init)
    | Pexp_apply(e1, es) ->
        let (fs, e_f) = norm_in e1 in
        let lss = (fs, e_f)::(List.map norm_in es) in
        let ts = List.map (fun e -> fresh_ident ()) es in
        let fn = fresh_ident () in
        let init = mk_let Nonrecursive fn e_f (mk_dum_apply fn ts) in

        let f (ls, e_in) b =
          let init = mk_dum_let Nonrecursive 

    | e -> assert false

  and norm_in = 
         
    



in
norm_out exp



