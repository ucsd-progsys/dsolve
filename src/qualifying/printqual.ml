open Outcometree
open Format
open Location

let qualifier_names quals = List.map (fun (p, _, _) -> Path.name p) quals

let rec qualify_tree_of_type_scheme otyp fr =
  let qualify = qualify_tree_of_type_scheme in
  match (otyp, fr) with
      (Otyp_arrow(l, t1, t2), Frame.Farrow(_, f1, f2)) ->
        Otyp_arrow(l, qualify t1 f1, qualify t2 f2)
    | (Otyp_constr(id, tyl, None), Frame.Fconstr(_, fl, _, (_, Frame.Qconst quals))) ->
        let qualifier_num = [string_of_int (List.length quals)] in
        let qualifier_desc = if !Clflags.brief_quals then qualifier_num else
                              qualifier_names quals in
        Otyp_constr(id, List.map2 qualify tyl fl, Some qualifier_desc)
    | (Otyp_constr (id, tyl, _), Frame.Frecord (_, _, (_, Frame.Qconst quals))) ->
        Otyp_constr (id, tyl, Some (qualifier_names quals))
    | (Otyp_var _, Frame.Fvar _) ->
        otyp
    | (Otyp_tuple (ts, _), Frame.Ftuple (fs, (_, Frame.Qconst quals))) ->
        Otyp_tuple(List.map2 qualify ts fs, Some (qualifier_names quals))
    | (_, Frame.Funknown) ->
        otyp
    | _ -> assert false
