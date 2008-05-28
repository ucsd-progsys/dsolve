(* pmr: "this is tedious" -
   1) pattern matching needs to be desugared automatically
   2) we can't reference l1, l2 directly, we need to "re-construct" them in
      the later cases *)
let rec merge l1 l2 =
  match l1 with
    | []      -> l2
    | x :: xs ->
        match l2 with
          | []      -> l1
          | y :: ys ->
              if x < y then
                x :: (merge xs (y :: ys))
              else
                y :: (merge (x :: xs) ys)

let rec split_aux lst left right = match lst with
  | []      -> (left, right)
  | x :: xs ->
      match xs with
        | []      -> (x :: left, right)
        | y :: ys -> split_aux ys (x :: left) (y :: right)

let split lst =
  split_aux lst [] []

let rec mergesort lst =
  match lst with
    | []  -> []
    | x :: xs ->
        match xs with
          | [] -> [x]
          | _ ->
              let (left, right) = split lst in
                merge (mergesort left) (mergesort right)

let rec check l =
  match l with
    | [] -> ()
    | x :: xs ->
        match xs with
          | [] -> ()
          | y :: ys ->
              assert (x <= y); check (y :: ys)

let test l =
  check (mergesort l)
