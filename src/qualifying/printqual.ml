open Outcometree
open Format
open Location

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
    | (Otyp_constr _ as ot, Frame.Frecord _) ->
        ot
    | (Otyp_var _, Frame.Fvar _) ->
        otyp
    | (Otyp_tuple ts, Frame.Ftuple fs) ->
        Otyp_tuple(List.map2 qualify ts fs)
    | (_, Frame.Funknown) ->
        otyp
    | _ -> assert false

let dump_frame pp loc fr =
  Stypes.print_position pp loc.loc_start;
  fprintf pp " ";
  Stypes.print_position pp loc.loc_end;
  fprintf pp "@.type(@.  ";
  Frame.pprint pp fr;
  fprintf pp "@.)@."

let dump filename framemap =
  let pp = formatter_of_out_channel (open_out filename) in
    Qualifymod.LocationMap.iter (dump_frame pp) framemap
