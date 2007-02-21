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
type expr =
    Num of int
  | True
  | False
  | Var of string
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


let eval exp =
  let rec eval_rec exp env =
    match exp with
	Num(n) -> NumVal(n)
      | True -> TrueVal
      | False -> FalseVal
      | Var(x) -> env_lookup x env
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