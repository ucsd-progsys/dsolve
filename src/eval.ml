open Expr


type value =
    NumVal of int
  | ListVal of value list
  | Closure of string * expression * string option * (string * value) list


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


let rec pprint_value = function
    NumVal n ->
      string_of_int n
  | ListVal l ->
      Printf.sprintf "[%s]" (Misc.join (List.map pprint_value l) "; ")
  | Closure(name, _, _, _) ->
      "<fun (" ^ name ^ ")>"
