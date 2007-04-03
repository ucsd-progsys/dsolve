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
type binoper =
    Plus
  | Minus
  | Times
  | Equal
  | Less

type expr =
    Num of int
  | True
  | False
  | Var of string
  | BinOp of binoper * expr * expr
  | If of expr * expr * expr
  | Annot of qual * expr
  | Let of string * typ option * expr * expr
  | Abs of string * typ option * expr
  | App of expr * expr
  | TyAbs of string * expr
  | TyApp of expr * typ
  | QualAbs of string * qual * expr
  | QualApp of expr * qual


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
	Num(n) -> NumVal(n)
      | True -> TrueVal
      | False -> FalseVal
      | Var(x) -> env_lookup x env
      | BinOp(o, e1, e2) ->
	  let (v1, v2) = (eval_rec e1 env, eval_rec e2 env) in
	    begin match (o, v1, v2) with
		(Plus, NumVal n1, NumVal n2) ->
		  NumVal (n1 + n2)
	      | (Minus, NumVal n1, NumVal n2) ->
		  NumVal (n1 - n2)
	      | (Times, NumVal n1, NumVal n2) ->
		  NumVal (n1 * n2)
	      | (Equal, _, _) ->
		  bool_to_val (v1 = v2)
	      | (Less, NumVal n1, NumVal n2) ->
		  bool_to_val (n1 < n2)
	      | _ ->
		  raise BogusEvalError
	    end
      | If(c, e1, e2) ->
	  begin match (eval_rec c env) with
	      TrueVal -> eval_rec e1 env
	    | FalseVal -> eval_rec e2 env
	    | _ -> raise BogusEvalError
	  end
      | Annot(_, e) -> eval_rec e env
      | Let(x, _, e, e') ->
	  let xv = eval_rec e env in
	  let newenv = env_add x xv env in
	    eval_rec e' newenv
      | Abs(x, _, e) -> Closure(x, e, env)
      | App(e1, e2) ->
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


let pprint_value = function
    NumVal n ->
      string_of_int n
  | TrueVal ->
      "true"
  | FalseVal ->
      "false"
  | Closure(name, _, _) ->
      "<fun (" ^ name ^ ")>"
