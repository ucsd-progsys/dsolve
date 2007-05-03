open Env


(* Type language *)
type qual =
    Top
  | Bottom
  | Qual of string
  | QualVar of string
  | QualMeet of qual * qual
  | QualJoin of qual * qual


type typ =
    Arrow of qual * typ * typ
  | Int of qual
  | Bool of qual
  | TyVar of qual * string
  | Nil
  | ForallQual of string * qual * typ
  | ForallTyp of string * typ


(* Expression language *)
type binop =
    Plus
  | Minus
  | Times

type binrel =
    Eq
  | Ne
  | Lt
  | Le 

type expr_id = string

type expr =
    Num of int * expr_id
  | TrueExp of expr_id
  | FalseExp of expr_id
  | ExpVar of string * expr_id
  | BinOp of binop * expr * expr * expr_id
  | BinRel of binrel * expr * expr * expr_id
  | If of expr * expr * expr * expr_id
  | Annot of qual * expr * expr_id
  | Let of string * typ option * expr * expr * expr_id
  | Abs of string * typ option * expr * expr_id
  | App of expr * expr * expr_id
  | Fix of string * expr * expr_id
  | TyAbs of string * expr * expr_id
  | TyApp of expr * typ * expr_id
  | QualAbs of string * qual * expr * expr_id
  | QualApp of expr * qual * expr_id


(* Values resulting from evalutation *)
type value =
    NumVal of int
  | TrueVal
  | FalseVal
  | Closure of string * expr * string option * value env


(* Expression evaluator *)
exception BogusEvalError


let bool_to_val b =
  if b then
    TrueVal
  else
    FalseVal


let eval exp =
  let rec eval_rec exp env =
    match exp with
	Num(n, _) -> NumVal(n)
      | TrueExp(_) -> TrueVal
      | FalseExp(_) -> FalseVal
      | ExpVar(x, _) -> env_lookup x env
      | BinOp(o, e1, e2, _) ->
	  let (v1, v2) = (eval_rec e1 env, eval_rec e2 env) in
	    begin match (o, v1, v2) with
		(Plus, NumVal n1, NumVal n2) ->
		  NumVal (n1 + n2)
	      | (Minus, NumVal n1, NumVal n2) ->
		  NumVal (n1 - n2)
	      | (Times, NumVal n1, NumVal n2) ->
		  NumVal (n1 * n2)
	      | _ ->
		  raise BogusEvalError
	    end
      | BinRel(r, e1, e2, _) ->
	  let (v1, v2) = (eval_rec e1 env, eval_rec e2 env) in
	    begin match (r, v1, v2) with
		(Eq, _, _) ->
		  bool_to_val (v1 = v2)
	      | (Lt, NumVal n1, NumVal n2) ->
		  bool_to_val (n1 < n2)
	      | (Le, NumVal n1, NumVal n2) ->
		  bool_to_val (n1 <= n2)
	      | (Ne, _, _) ->
		  bool_to_val (v1 != v2)
	      | _ ->
		  raise BogusEvalError
	    end
      | If(c, e1, e2, _) ->
	  begin match (eval_rec c env) with
	      TrueVal -> eval_rec e1 env
	    | FalseVal -> eval_rec e2 env
	    | _ -> raise BogusEvalError
	  end
      | Annot(_, e, _) -> eval_rec e env
      | Let(x, _, e, e', _) ->
	  let xv = eval_rec e env in
	  let newenv = env_add x xv env in
	    eval_rec e' newenv
      | Abs(x, _, e, _) -> Closure(x, e, None, env)
      | App(e1, e2, _) ->
	  let e2' = eval_rec e2 env in
	    begin match eval_rec e1 env with
		Closure(x, e, fix, cenv) as c ->
		  let fixenv = match fix with
		      None ->
			cenv
		    | Some f ->
			env_add f c cenv
		  in
		  let newenv = env_add x e2' fixenv in
		    eval_rec e newenv
	      | _ -> raise BogusEvalError
	    end
	      (* XXX: needs type/qual abs and apps *)
      | Fix(f, e, _) ->
	  let e' = eval_rec e env in
	    begin match e' with
		Closure(x, e, None, env) ->
		  Closure(x, e, Some f, env)
	      | _ ->
		  e'
	    end
      | _ -> raise BogusEvalError
  in
    eval_rec exp []


let expr_get_id = function
    Num(_, id)
  | TrueExp(id)
  | FalseExp(id)
  | ExpVar(_, id)
  | BinOp(_, _, _, id)
  | BinRel(_, _, _, id)
  | If(_, _, _, id)
  | Annot(_, _, id)
  | Let(_, _, _, _, id)
  | Abs(_, _, _, id)
  | App(_, _, id)
  | Fix(_, _, id)
  | TyAbs(_, _, id)
  | TyApp(_, _, id)
  | QualAbs(_, _, _, id)
  | QualApp(_, _, id) ->
      id


let expr_get_subexprs = function
    Num(_, _)
  | TrueExp(_)
  | FalseExp(_)
  | ExpVar(_) ->
      []
  | BinOp(_, e1, e2, _)
  | BinRel(_, e1, e2, _) ->
      [e1; e2]
  | If(e1, e2, e3, _) ->
      [e1; e2; e3]
  | Annot(_, e, _) ->
      [e]
  | Let(_, _, e1, e2, _) ->
      [e1; e2]
  | Abs(_, _, e, _) ->
      [e]
  | App(e1, e2, _) ->
      [e1; e2]
  | Fix(_, e, _) ->
      [e]
  | TyAbs(_, e, _) ->
      [e]
  | TyApp(e, _, _) ->
      [e]
  | QualAbs(_, _, e, _) ->
      [e]
  | QualApp(e, _, _) ->
      [e]


let pprint_binop frec e1 op e2 =
  let opstr =
    match op with
	Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
  in
  let (str1, str2) = (frec e1, frec e2) in
    Misc.join [str1; opstr; str2] " "


let pprint_binrel frec e1 rel e2 =
  let relstr =
    match rel with
	Eq -> "="
      | Ne -> "!="
      | Lt -> "<"
      | Le -> "<="
  in
  let (str1, str2) = (frec e1, frec e2) in
    Misc.join [str1; relstr; str2] " "


let rec pprint_annotated_expr annotator exp =
  let pprint_rec = pprint_annotated_expr annotator in
  let pprint_expr_simple = function
      Num(n, _) ->
	string_of_int n
    | TrueExp _ ->
	"true"
    | FalseExp _ ->
	"false"
    | ExpVar(x, _) ->
	x
    | BinOp(op, e1, e2, _) ->
	pprint_binop pprint_rec e1 op e2
    | BinRel(rel, e1, e2, _) ->
	pprint_binrel pprint_rec e1 rel e2
    | If(e1, e2, e3, _) ->
	"if " ^ pprint_rec e1 ^ " then " ^ pprint_rec e2 ^ " else " ^ pprint_rec e3
    | Annot(Qual(q), e, _) ->
	"({[" ^ q ^ "]} " ^ pprint_rec e ^ ")"
    | Let(x, _, e1, e2, _) ->
	"let " ^ x ^ " = " ^ pprint_rec e1 ^ " in " ^ pprint_rec e2
    | Abs(x, _, e, _) ->
	"fun " ^ x ^ " = " ^ pprint_rec e
    | App(e1, e2, _) ->
	pprint_rec e1 ^ " " ^ pprint_rec e2
    | _ ->
	""
  in
  let quals = annotator exp in
  let qualstrs = List.map (fun s -> "{[" ^ s ^ "]}") quals in
    "(" ^ (Misc.join qualstrs " ") ^ " " ^ pprint_expr_simple exp ^ ")"


let pprint_value = function
    NumVal n ->
      string_of_int n
  | TrueVal ->
      "true"
  | FalseVal ->
      "false"
  | Closure(name, _, _, _) ->
      "<fun (" ^ name ^ ")>"
