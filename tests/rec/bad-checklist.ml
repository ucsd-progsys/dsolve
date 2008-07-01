let rec check l =
  match l with
    | [] -> ()
    | x :: [] -> ()
    | x :: y :: ys ->
        assert (x <= y); check (y :: ys)

let _ = check [5; 2; 3]
