open Outcometree
open Frame

let rec qualify_tree_of_type_scheme otyp fr =
  let qualify = qualify_tree_of_type_scheme in
  match (otyp, fr) with
      (Otyp_arrow(l, t1, t2), Farrow(_, f1, f2)) ->
        Otyp_arrow(l, qualify t1 f1, qualify t2 f2)
    | (Otyp_constr(id, tyl, None), Fconstr(_, fl, (_, Qconst quals))) ->
        let qualifier_names = List.map (fun (p, _, _) -> Path.name p) quals in
        Otyp_constr(id, List.map2 qualify tyl fl, Some (Oqual qualifier_names))
    | (Otyp_var _, Fvar _) ->
        otyp
    | _ -> assert false
