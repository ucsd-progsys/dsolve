open Asttypes
open Longident
open Parsetree
open Predicate


module Expression = struct
  type t = expression
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (==)
end

module ExpMap = Map.Make(Expression)


exception ExpressionNotSupported


let expression_to_pexpr e =
  match e.pexp_desc with
    | Pexp_constant(Const_int n) ->
	PInt n
    | Pexp_ident(Lident x) ->
	Var x
    | _ -> fresh_pexprvar()


let expr_builtin_qualifier exp =
  match exp.pexp_desc with
    | Pexp_constant(Const_int n) ->
        Some(Builtins.equality_qualifier (PInt n))
    | Pexp_ident(Lident x) ->
        if Env.mem x Builtins.types then
          None
        else
          Some(Builtins.equality_qualifier (Var x))
    | _ -> None


let exprs_of_expr_pairs pexps = List.map (fun (_, exp) -> exp) pexps


let expr_get_subexprs e =
  match e.pexp_desc with
    | Pexp_ident _
    | Pexp_constant _
    | Pexp_construct(_, None, _) ->
	[]
    | Pexp_ifthenelse(e1, e2, Some e3) ->
	[e1; e2; e3]
    | Pexp_let(_, pexps, e) ->
	e::(exprs_of_expr_pairs pexps)
    | Pexp_function(_, _, pexps) ->
	exprs_of_expr_pairs pexps
    | Pexp_apply(e, lexps) ->
	e::(exprs_of_expr_pairs lexps)
    | Pexp_construct(_, Some e, _) ->
	[e]
    | Pexp_tuple es ->
	es
    | _ -> raise ExpressionNotSupported


let rec expr_map f e =
  let rv = f e in
  let rest = Misc.flap (expr_map f) (expr_get_subexprs e) in
    rv::rest


let expression_required_builtin_quals exp =
  (* pmr: gross misuse of mapfilter - some expressions are going to return None if
     they have no built-in quals *)
  let quals = expr_map expr_builtin_qualifier exp in Misc.mapfilter (fun x -> x) quals


let rec pprint_annotated_expression annotator indent exp =
  let indstr = String.make indent ' ' in
  let pprint_rec = pprint_annotated_expression annotator 0 in
  let pprint_ind = pprint_annotated_expression annotator (indent + 2) in
  let pprint_expr_simple e =
    match e.pexp_desc with
      | Pexp_constant(Const_int n) ->
	string_of_int n
      | Pexp_ident(Lident x) ->
	  x
      | Pexp_construct(Lident "[]", _, _) ->
	  "[]"
      | Pexp_construct(Lident "::", Some {pexp_desc = Pexp_tuple [e1; e2]}, _) ->
          Printf.sprintf "%s::%s" (pprint_rec e1) (pprint_rec e2)
      | Pexp_ifthenelse(c, e1, Some e2) ->
	  Printf.sprintf "if %s then\n%s\n%selse\n%s\n" (pprint_rec c) (pprint_ind e1) indstr (pprint_ind e2)
(*    | Exp.Match(e1, e2, (h, t), e3, _) ->
        Printf.sprintf "match %s with\n%sNil ->\n%s\n%s| Cons(%s, %s) ->\n%s"
          (pprint_rec e1) indstr (pprint_ind e2) indstr h t (pprint_ind e3) *)
      | Pexp_let(recflag, [({ppat_desc = Ppat_var x}, e1)], e2) ->
	  Printf.sprintf "let %s %s = %s in\n%s\n"
	    (match recflag with Recursive -> "rec" | _ -> "") x (pprint_rec e1) (pprint_ind e2)
      | Pexp_function(_, _, [({ppat_desc = Ppat_var x}, e)]) ->
	  Printf.sprintf "fun %s ->\n%s\n" x (pprint_ind e)
      | Pexp_apply(e, exps) ->
	  Printf.sprintf "%s %s"
	    (pprint_rec e) (Misc.join (List.map (fun (_, e) -> pprint_rec e) exps) " ")
      | _ -> raise ExpressionNotSupported
  in Printf.sprintf "%s(%s: %s)" (String.make indent ' ') (pprint_expr_simple exp) (annotator exp)


let pprint_expression =
  pprint_annotated_expression (fun e -> "") 0
