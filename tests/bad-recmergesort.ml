let rec merge l1 l2 =
  match (l1, l2) with
    | ([], l2) -> l2
    | (l1, []) -> l1
    | (x :: xs, y :: ys) ->
        if x < y then
          y :: (merge xs (y :: ys))
        else
          y :: (merge (x :: xs) ys)

let rec split_aux lst left right = match lst with
  | []           -> (left, right)
  | [x]          -> (x :: left, right)
  | x :: y :: ys -> split_aux ys (x :: left) (y :: right)

let split lst =
  split_aux lst [] []

let rec mergesort lst =
  match lst with
    | []      -> []
    | [x]     -> [x]
    | x :: xs ->
        let (left, right) = split lst in
          merge (mergesort left) (mergesort right)

let rec check l =
  match l with
    | [] -> ()
    | x :: [] -> ()
    | x :: y :: ys ->
        assert (x <= y); check (y :: ys)

let test l =
  check (mergesort l)
