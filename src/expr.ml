open Type
open Predicate


(* Expression language *)
type expr_id = string

type expr =
    Num of int * expr_id
  | ExpVar of string * expr_id
  | If of expr * expr * expr * expr_id
  | Let of string * typ option * expr * expr * expr_id
  | Abs of string * typ option * expr * expr_id
  | App of expr * expr * expr_id


(* Values resulting from evalutation *)
type value =
    NumVal of int
  | Closure of string * expr * string option * (string * value) list


(* Expression evaluator *)
exception BogusEvalError


let bool_to_val b =
  if b then
    NumVal 1
  else
    NumVal 0
	  

let eval exp =
  let rec eval_rec exp env =
    match exp with
	Num(n, _) ->
	  NumVal(n)
      | ExpVar(x, _) ->
	  List.assoc x env
      | If(c, e1, e2, _) ->
	  begin match (eval_rec c env) with
	      NumVal 0 -> eval_rec e2 env
	    | NumVal 1 -> eval_rec e1 env
	    | _ -> raise BogusEvalError
	  end
      | Let(x, _, e, e', _) ->
	  let xv = eval_rec e env in
	  let newenv = (x, xv)::env in
	    eval_rec e' newenv
      | Abs(x, _, e, _) ->
	  Closure(x, e, None, env)
      | App(e1, e2, _) ->
	  let e2' = eval_rec e2 env in
	    begin match eval_rec e1 env with
		Closure(x, e, fix, cenv) as c ->
		  let fixenv = match fix with
		      None ->
			cenv
		    | Some f ->
			(f, c)::cenv
		  in
		  let newenv = (x, e2')::fixenv in
		    eval_rec e newenv
	      | _ -> raise BogusEvalError
	    end
  in
    eval_rec exp []


let expr_get_id = function
    Num(_, id)
  | ExpVar(_, id)
  | If(_, _, _, id)
  | Let(_, _, _, _, id)
  | Abs(_, _, _, id)
  | App(_, _, id) ->
      id


let expr_get_subexprs = function
    Num(_, _)
  | ExpVar(_) ->
      []
  | If(e1, e2, e3, _) ->
      [e1; e2; e3]
  | Let(_, _, e1, e2, _) ->
      [e1; e2]
  | Abs(_, _, e, _) ->
      [e]
  | App(e1, e2, _) ->
      [e1; e2]


let rec expr_map f e =
  let rv = f e in
  let rest = Misc.flap (expr_map f) (expr_get_subexprs e) in
    rv::rest


let expr_to_predicate_expression = function
    Num(n, _) ->
      PInt n
  | ExpVar(x, _) ->
      Var x
  | _ ->
      fresh_expressionvar()


let rec pprint_annotated_expr annotator indent exp =
  let indstr = String.make indent ' ' in
  let pprint_rec = pprint_annotated_expr annotator 0 in
  let pprint_ind = pprint_annotated_expr annotator (indent + 2) in
  let pprint_expr_simple e =
    match e with
      Num(n, _) ->
	string_of_int n
    | ExpVar(x, _) ->
	x
    | If(e1, e2, e3, _) ->
	Printf.sprintf "if %s then\n%s\n%selse\n%s" (pprint_rec e1) (pprint_ind e2) indstr (pprint_ind e3)
    | Let(x, _, e1, e2, _) ->
	Printf.sprintf "let %s = %s in\n%s" x (pprint_rec e1) (pprint_ind e2)
    | Abs(x, _, e, _) ->
	Printf.sprintf "fun %s ->\n%s" x (pprint_ind e)
    | App(e1, e2, _) ->
	Printf.sprintf "%s (%s)" (pprint_rec e1) (pprint_rec e2)
  in
  let quals = annotator exp in
  let qualstrs = List.map (fun s -> "{" ^ s ^ "}") quals in
  let expstr = pprint_expr_simple exp in
  let qualexpstr =
    match qualstrs with
	[] -> expstr
      | _ ->
	  Printf.sprintf "(%s %s)" (Misc.join qualstrs " ") expstr
  in
    Printf.sprintf "%s%s" (String.make indent ' ') qualexpstr


let pprint_expr =
  pprint_annotated_expr (fun e -> []) 0


let rec pprint_value = function
    NumVal n ->
      string_of_int n
  | Closure(name, _, _, _) ->
      "<fun (" ^ name ^ ")>"
