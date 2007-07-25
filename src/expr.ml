open Type
open Predicate


let next_exp_id = ref 0
let get_next_exp_id () =
  incr next_exp_id;
  string_of_int !next_exp_id


(* Expression language *)
module Exp = struct
  type expr_id = string

  type t =
      Num of int * expr_id
    | Var of string * expr_id
    | Nil of expr_id
    | Cons of t * t * expr_id
    | If of t * t * t * expr_id
    | Match of t * t * (string * string) * t * expr_id
    | Let of string * typ option * t * t * expr_id
    | LetRec of string * typ option * t * t * expr_id
    | Abs of string * typ option * t * expr_id
    | App of t * t * expr_id

  let compare = compare
  let hash = Hashtbl.hash
  let equal = (==)
end


module ExpMap = Map.Make(Exp)

(* Values resulting from evalutation *)
type value =
    NumVal of int
  | ListVal of value list
  | Closure of string * Exp.t * string option * (string * value) list


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
	Exp.Num(n, _) ->
	  NumVal n
      | Exp.Var(x, _) ->
	  List.assoc x env
      | Exp.Nil _ ->
          ListVal []
      | Exp.Cons(e1, e2, _) ->
          begin match eval_rec e2 env with
              ListVal l ->
                ListVal ((eval_rec e1 env)::l)
            | _ -> raise BogusEvalError
          end
      | Exp.If(c, e1, e2, _) ->
	  begin match (eval_rec c env) with
	      NumVal 0 -> eval_rec e2 env
	    | NumVal 1 -> eval_rec e1 env
	    | _ -> raise BogusEvalError
	  end
      | Exp.Match(e1, e2, (h, t), e3, _) ->
          begin match eval_rec e1 env with
              ListVal([]) ->
                eval_rec e2 env
            | ListVal(hd::tl) ->
                eval_rec e3 ((h, hd)::(t, ListVal tl)::env)
            | _ ->
                raise BogusEvalError
          end
      | Exp.Let(x, _, e, e', _) ->
	  let xv = eval_rec e env in
	  let env' = (x, xv)::env in
	    eval_rec e' env'
      | Exp.LetRec(f, _, Exp.Abs(x, _, e, _), e', _) ->
          let fv = Closure(x, e, Some f, env) in
          let env' = (f, fv)::env in
            eval_rec e' env'
      | Exp.LetRec _ ->
          raise BogusEvalError
      | Exp.Abs(x, _, e, _) ->
	  Closure(x, e, None, env)
      | Exp.App(e1, e2, _) ->
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
  in eval_rec exp []


let expr_to_predicate_expression = function
    Exp.Num(n, _) ->
      PInt n
  | Exp.Var(x, _) ->
      Var x
  | _ -> fresh_expressionvar()


let expr_builtin_qualifier exp =
  match exp with
      Exp.Num(n, _) ->
        Some(Builtins.equality_qualifier (PInt n))
    | Exp.Var(x, _) ->
        if Env.mem x Builtins.types then
          None
        else
          Some(Builtins.equality_qualifier (Var x))
    | _ -> None


let expr_get_subexprs = function
    Exp.Num _
  | Exp.Var _
  | Exp.Nil _ ->
      []
  | Exp.Cons(e1, e2, _) ->
      [e1; e2]
  | Exp.If(e1, e2, e3, _) ->
      [e1; e2; e3]
  | Exp.Match(e1, e2, _, e3, _) ->
      [e1; e2; e3]
  | Exp.Let(_, _, e1, e2, _) ->
      [e1; e2]
  | Exp.LetRec(_, _, e1, e2, _) ->
      [e1; e2]
  | Exp.Abs(_, _, e, _) ->
      [e]
  | Exp.App(e1, e2, _) ->
      [e1; e2]


let rec expr_map f e =
  let rv = f e in
  let rest = Misc.flap (expr_map f) (expr_get_subexprs e) in
    rv::rest


let expr_required_builtin_quals exp =
  (* pmr: gross misuse of mapfilter - some expressions are going to return None if
     they have no built-in quals *)
  let quals = expr_map expr_builtin_qualifier exp in Misc.mapfilter (fun x -> x) quals


let rec pprint_annotated_expr annotator indent exp =
  let indstr = String.make indent ' ' in
  let pprint_rec = pprint_annotated_expr annotator 0 in
  let pprint_ind = pprint_annotated_expr annotator (indent + 2) in
  let pprint_expr_simple e =
    match e with
      Exp.Num(n, _) ->
	string_of_int n
    | Exp.Var(x, _) ->
	x
    | Exp.Nil _ ->
        "Nil"
    | Exp.Cons(e1, e2, _) ->
        Printf.sprintf "%s::%s" (pprint_rec e1) (pprint_rec e2)
    | Exp.If(e1, e2, e3, _) ->
	Printf.sprintf "if %s then\n%s\n%selse\n%s\n" (pprint_rec e1) (pprint_ind e2) indstr (pprint_ind e3)
    | Exp.Match(e1, e2, (h, t), e3, _) ->
        Printf.sprintf "match %s with\n%sNil ->\n%s\n%s| Cons(%s, %s) ->\n%s"
          (pprint_rec e1) indstr (pprint_ind e2) indstr h t (pprint_ind e3)
    | Exp.Let(x, _, e1, e2, _) ->
	Printf.sprintf "let %s = %s in\n%s\n" x (pprint_rec e1) (pprint_ind e2)
    | Exp.LetRec(f, _, e1, e2, _) ->
        Printf.sprintf "letrec %s = %s in\n%s\n" f (pprint_rec e1) (pprint_rec e2)
    | Exp.Abs(x, _, e, _) ->
	Printf.sprintf "fun %s ->\n%s\n" x (pprint_ind e)
    | Exp.App(e1, e2, _) ->
	Printf.sprintf "%s %s" (pprint_rec e1) (pprint_rec e2)
  in
  let annot = annotator exp in
  let expstr = pprint_expr_simple exp in
    Printf.sprintf "%s(%s: %s)" (String.make indent ' ') expstr annot


let pprint_expr =
  pprint_annotated_expr (fun e -> "") 0


let rec pprint_value = function
    NumVal n ->
      string_of_int n
  | ListVal l ->
      Printf.sprintf "[%s]" (Misc.join (List.map pprint_value l) "; ")
  | Closure(name, _, _, _) ->
      "<fun (" ^ name ^ ")>"
