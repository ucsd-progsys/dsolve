let rec mlength l =
  match l with
    | [] -> 0
    | _ :: t -> 1 + mlength t

let test2 l2_1 =
  match l2_1 with
    | [] ->
        assert (0 = (mlength l2_1) * (mlength l2_1));
        assert (0 = (mlength l2_1) * (mlength l2_1))
    | h :: t -> ()

