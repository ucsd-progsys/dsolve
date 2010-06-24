let rec mlength l =
  match l with
    | [] -> 0
    | _ :: t -> 1 + mlength t

let rec test1 l1_1 (c1_1 : int) =
  let len = mlength l1_1 in
    c1_1;;

let f_assert1 l_a1_1 c_a1_1 =
  let c_a1_2 = test1 l_a1_1 c_a1_1 in
    assert(c_a1_2 <= c_a1_1 + (mlength l_a1_1));
    assert(c_a1_2 <= c_a1_1 + (mlength l_a1_1) * (mlength l_a1_1))
