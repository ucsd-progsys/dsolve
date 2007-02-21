type 'a envvar = EnvVar of string * 'a

type 'a env = 'a envvar list


(* Environment handing *)
exception UnboundVar of string


let rec env_lookup x =
  function
      EnvVar(s, v)::l -> if s = x then v else env_lookup x l
    | _ -> raise (UnboundVar x)


let env_add x v env =
  EnvVar(x, v)::env