(* a small routine to parse a string as a toplevel stmt *)

open Parsetree

let (|>) x f = f x
let (|>>) x f = match x with Some x -> f x | None -> None
let (||>) x f = match x with Some x -> Some (f x) | None -> None

let extract_structure = function
  | Ptop_def str -> Some str
  | Ptop_dir _ -> None

let extract_desc = function
  | [x] -> Some x.pstr_desc
  | _   -> None
  (* toplevel parsing only yields a single str item *)

let extract_expr_from_val = function
  | Pstr_value (_, ps) -> snd (List.hd ps)
  | Pstr_eval e -> e  
  | _ -> assert false

let error_on_none msg = function
  | Some x -> x
  | None -> Printf.printf msg; flush stdout; assert false 

let quote s =
  Lexing.from_string (s ^ ";;")
  |>  Parse.toplevel_phrase
  |>  extract_structure
  |>> extract_desc
  |>  error_on_none "tried to quote toplevel directive"

let quote_expr s =
  quote ("let _ = " ^ s)
  |> extract_expr_from_val

let rec map_expr_desc f expr =
  let map = map_expr f in
  let map_pairs x = List.map (fun (p, e) -> (p, map e)) x in
  match expr with
  | Pexp_ident _ -> f expr
  | Pexp_constant _ -> f expr
  | Pexp_let (r, ps, e) ->
      f (Pexp_let (r, map_pairs ps, map e))
  | Pexp_function (l, eo, ps) ->
      f (Pexp_function (l, eo ||> map, map_pairs ps))
  | Pexp_apply (e, ls) ->
      f (Pexp_apply (map e, map_pairs ls))
  | Pexp_match (e, ps) ->
      f (Pexp_match (map e, map_pairs ps))
  | Pexp_try (e, ps) ->
      f (Pexp_try (map e, map_pairs ps))
  | Pexp_tuple es ->
      f (Pexp_tuple (List.map map es))
  | Pexp_construct (id, eo, b) ->
      f (Pexp_construct (id, eo ||> map, b))
  | Pexp_variant (l, eo) ->
      f (Pexp_variant (l, eo ||> map))
  | Pexp_record (ls, eo) ->
      f (Pexp_record (map_pairs ls, eo ||> map))
  | Pexp_field (e, id) ->
      f (Pexp_field (map e, id))
  | Pexp_setfield (e, id, e') ->
      f (Pexp_setfield (map e, id, map e'))
  | Pexp_array (es) ->
      f (Pexp_array (List.map map es))
  | Pexp_ifthenelse (e1, e2, eo) ->
      f (Pexp_ifthenelse (map e1, map e2, eo ||> map))
  | Pexp_sequence (e1, e2) ->
      f (Pexp_sequence (map e1, map e2))
  | Pexp_while (e1, e2) ->
      f (Pexp_while (map e1, map e2))
  | Pexp_for (s, e1, e2, d, e3) ->
      f (Pexp_for (s, map e1, map e2, d, map e3))
  | Pexp_constraint (e, t1, t2) ->
      f (Pexp_constraint (map e, t1, t2))
  | Pexp_when (e1, e2) ->
      f (Pexp_when (map e1, map e2))
  | Pexp_send (e, s) ->
      f (Pexp_send (map e, s))
  | Pexp_new id -> f expr
  | Pexp_setinstvar (s, e) ->
      f (Pexp_setinstvar (s, map e))
  | Pexp_override es ->
      f (Pexp_override (map_pairs es))
  | Pexp_letmodule (s, m, e) ->
      f (Pexp_letmodule (s, m, map e))
  | Pexp_assert e ->
      f (Pexp_assert (map e))
  | Pexp_assertfalse ->
      f expr
  | Pexp_assume e ->
      f (Pexp_assume (map e))
  | Pexp_lazy e ->
      f (Pexp_lazy (map e))
  | Pexp_poly (e, co) ->
      f (Pexp_poly (map e, co))
  | Pexp_object c ->
      f (Pexp_object c)
   
and map_expr f pexpr =
  { pexp_desc = map_expr_desc f pexpr.pexp_desc;
    pexp_loc  = pexpr.pexp_loc }
