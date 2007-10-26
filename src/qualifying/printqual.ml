open Outcometree

let rec qualify_tree_of_type_scheme otyp fr =
  let qualify = qualify_tree_of_type_scheme in
  match (otyp, fr) with
      (Otyp_arrow(l, t1, t2), Frame.Farrow(_, f1, f2)) ->
        Otyp_arrow(l, qualify t1 f1, qualify t2 f2)
    | (Otyp_constr(id, tyl, None), Frame.Fconstr(_, fl, (_, Frame.Qconst quals))) ->
        let qualifier_names = List.map (fun (p, _, _) -> Path.name p) quals in
        let qualifier_num = [string_of_int (List.length quals)] in
        let qualifier_desc = if !Clflags.brief_quals then qualifier_num else
                              qualifier_names in
        Otyp_constr(id, List.map2 qualify tyl fl, Some (Oqual qualifier_desc))
    | (Otyp_var _, Frame.Fvar _) ->
        otyp
    | (Otyp_tuple ts, Frame.Ftuple(t1', t2')) ->
        (* placeholder implementation *)
        let t1 = List.hd ts in
        let t2 = List.hd (List.tl ts) in
        Otyp_tuple([qualify t1 t1'; qualify t2 t2'])
    | (_, Frame.Funknown) ->
        otyp
    | _ -> assert false
