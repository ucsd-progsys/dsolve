(*
  - Description: Dsolve fails to prove number of apps of [compare]

  - Motivation: i.031, [assoc]

  - Modes afected: 0, other modes have not been tested.
*)

let rec mlength x =
  match x with
    [] -> 0
  | _ :: t -> 1 + mlength t

let rec assoc x l1 y s =
  match l1 with
    |  [] -> (* raise Not_found, s *) assert false
    | (a,b)::l ->
        let x_16 = mlength l in
        if compare a x = 0 then (b, s) else assoc x l y (s + 1)

let f_assert x l y pre =
  let (_, pos) = assoc x l y pre in assert (pos <= pre + mlength l)
