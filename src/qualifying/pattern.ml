open Typedtree

module P = Predicate

let is_deep = function
  | Tpat_any
  | Tpat_var _ -> false
  | _ -> true

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

let rec fold f b p = match p with
  | Tpat_any
  | Tpat_var _ -> f b p
  | Tpat_tuple pats ->
      let b = List.fold_left (fold f) b (pattern_descs pats) in f b p
  | _ -> assert false

let env_bind tenv env pat frame = Lightenv.addn (bind tenv pat frame) env

let null_binding_fold b = function
  | Tpat_var x -> (Path.Pident x, P.Var (Path.mk_ident "z")) :: b
  | _ -> b

let null_binding b pat =
  fold null_binding_fold b pat

let bind_pexpr pat pexp =
  let rec bind_rec subs (pat, pexp) =
    match pat with
    | Tpat_any -> subs
    | Tpat_var x -> (Path.Pident x, pexp) :: subs
    | Tpat_tuple pats ->
      let pexps = Misc.mapi (fun pat i -> (pat.pat_desc, P.Proj(i, pexp))) pats in
        List.fold_left bind_rec subs pexps
    | _ -> null_binding_fold subs pat
  in bind_rec [] (pat, pexp)

let desugar_bind pat pexp =
  P.big_and (List.map (fun (x, exp) -> P.equals(P.Var x, exp)) (bind_pexpr pat pexp))

let rec same p1 p2 =
  match (p1, p2) with
  | (Tpat_var x, Tpat_var y) when x = y -> true
  | (Tpat_any, Tpat_any) -> true
  | (Tpat_tuple pats1, Tpat_tuple pats2) ->
      List.for_all2 same (pattern_descs pats1) (pattern_descs pats2)
  | _ -> false
