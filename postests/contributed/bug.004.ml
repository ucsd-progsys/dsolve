let rec mlength x =
  match x with
    [] -> 0
  | _ :: t -> 1 + mlength t

(* assert_11 succeeds with the following...
let rec compose_list l1_1 s_2 =
  match l1_1 with
    [] -> 1, s_2
  | h1_1 :: t1_1 ->
      let s_5 = s_2 + 1 in
      let (c, s_6) = compose_list t1_1 s_5 in
      1, s_6
*)


let rec compose_list l1_1 s_2 =
  match l1_1 with
    [] -> (fun a1_1 -> a1_1), s_2
  | h1_1 :: t1_1 ->
      let s_5 = s_2 + 1 in
      let (c, s_6) = compose_list t1_1 s_5 in
      (fun a1_2 -> h1_1 (c a1_2)), s_6


let assert_11 l pre =
  let (_, pos) = compose_list l pre in assert (pos = pre + mlength l)
