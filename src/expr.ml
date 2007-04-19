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

type expr_id = int

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
  | TyAbs of string * expr * expr_id
  | TyApp of expr * typ * expr_id
  | QualAbs of string * qual * expr * expr_id
  | QualApp of expr * qual * expr_id


(* Values resulting from evalutation *)
type value =
    NumVal of int
  | TrueVal
  | FalseVal
  | Closure of string * expr * value env


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
      | Abs(x, _, e, _) -> Closure(x, e, env)
      | App(e1, e2, _) ->
	  let e2' = eval_rec e2 env in
	    begin match eval_rec e1 env with
		Closure(x, e, cenv) ->
		  let newenv = env_add x e2' cenv in
		    eval_rec e newenv
	      | _ -> raise BogusEvalError
	    end
	      (* XXX: needs type/qual abs and apps *)
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
  | TyAbs(_, _, id)
  | TyApp(_, _, id)
  | QualAbs(_, _, _, id)
  | QualApp(_, _, id) ->
      id


let pprint_value = function
    NumVal n ->
      string_of_int n
  | TrueVal ->
      "true"
  | FalseVal ->
      "false"
  | Closure(name, _, _) ->
      "<fun (" ^ name ^ ")>"
