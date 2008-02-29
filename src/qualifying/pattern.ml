open Typedtree

let pattern_descs = List.map (fun p -> p.pat_desc)

let bind env pat frame =
  let rec bind_rec bindings pat frame =
    match (pat, frame) with
    | (Tpat_any, _) -> bindings
    | (Tpat_var x, f) -> (Path.Pident x, f) :: bindings
    | (Tpat_tuple pats, Frame.Ftuple (fs, _)) ->
        List.fold_left2 bind_rec bindings (pattern_descs pats) fs
    | (Tpat_construct (cstrdesc, pats), f) ->
        List.fold_left2 bind_rec bindings (pattern_descs pats)
          (Frame.fresh_constructor env cstrdesc f)
    | _ -> assert false
  in bind_rec [] pat frame

let env_bind tenv env pat frame = Lightenv.addn (bind tenv pat frame) env

let bind_pexpr pat pexp =
  let rec bind_rec subs (pat, pexp) =
    match pat with
    | Tpat_any -> subs
    | Tpat_var x -> (Path.Pident x, pexp) :: subs
    | Tpat_tuple pats ->
      let pexps = Misc.mapi (fun pat i -> (pat.pat_desc, Predicate.tuple_nth [pexp] i)) pats in
        List.fold_left bind_rec subs pexps
    | _ -> assert false
  in bind_rec [] (pat, pexp)

let rec same p1 p2 =
  match (p1, p2) with
  | (Tpat_var x, Tpat_var y) when x = y -> true
  | (Tpat_any, Tpat_any) -> true
  | (Tpat_tuple pats1, Tpat_tuple pats2) ->
    let pdesc p = p.pat_desc in
    let (pats1, pats2) = (List.map pdesc pats1, List.map pdesc pats2) in
      List.for_all2 same pats1 pats2
  | _ -> false
