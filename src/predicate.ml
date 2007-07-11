type binop =
    Plus
  | Minus
  | Times


type binrel =
    Eq
  | Ne
  | Lt
  | Le 


type expression =
    PInt of int 
  | Var of string
  | Pvar of string * int
  | Binop of expression * binop * expression 


type predicate =
    True
  | Atom of expression * binrel * expression 
  | Not of predicate
  | And of predicate * predicate 
  | Or of predicate * predicate


type parameterized_pred = PredOver of (string * predicate)


let equals(p, q) =
  Atom(p, Eq, q)


let big_and cs =
  let combine p q = And(p, q) in
    List.fold_right combine cs True


let big_or cs =
  let combine p q = Or(p, q) in
    List.fold_right combine cs (Not(True))


let rec predexpr_subst v x = function
    Var y when y = x ->
      v
  | Binop(e1, op, e2) ->
      Binop(predexpr_subst v x e1, op, predexpr_subst v x e2)
  | e ->
      e


let rec predicate_subst v x = function
    True ->
      True
  | Atom(e1, rel, e2) ->
      Atom(predexpr_subst v x e1, rel, predexpr_subst v x e2)
  | Not(p) ->
      Not(predicate_subst v x p)
  | And(p, q) ->
      And(predicate_subst v x p, predicate_subst v x q)
  | Or(p, q) ->
      Or(predicate_subst v x p, predicate_subst v x q)


let predicate_vars p =
  let rec exp_vars_rec vars = function
      PInt _ ->
        vars
    | Var x ->
        x::vars
    | Pvar(x, _) ->
        x::vars
    | Binop(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
  in
  let rec vars_rec vars = function
      True ->
        vars
    | Atom(e1, _, e2) ->
        let vars' = exp_vars_rec vars e1 in
          exp_vars_rec vars' e2
    | Not p ->
        vars_rec vars p
    | And(p1, p2)
    | Or(p1, p2) ->
        let vars' = vars_rec vars p1 in
          vars_rec vars' p2
  in
    vars_rec [] p


let implies(p, q) =
  Or(Not p, q)


let fresh_expressionvar = Misc.make_get_fresh (fun x -> Var ("__" ^ x))


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


let rec pprint_expression = function
    PInt(n) ->
      string_of_int n
  | Var x ->
      x
  | Pvar(x, n) ->
      x ^ "_" ^ (string_of_int n)
  | Binop(e1, op, e2) ->
      pprint_binop pprint_expression e1 op e2


let rec pprint_predicate = function
    True ->
      "true"
  | Atom(e1, rel, e2) ->
      "(" ^ pprint_binrel pprint_expression e1 rel e2 ^ ")"
  | Not(p) ->
      "(not " ^ pprint_predicate p ^ ")"
  | And(p, q) ->
      "(and " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
  | Or(p, q) ->
      "(or " ^ pprint_predicate p ^ " " ^ pprint_predicate q ^ ")"
