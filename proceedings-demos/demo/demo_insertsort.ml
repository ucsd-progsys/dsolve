let show x = ()

let elts = S.elts

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'

let rec insert ys x =
  match ys with
  | [] -> x :: []
  | y :: ys' ->
      if x < y 
      then x :: y :: ys'
      else y :: (insert ys' x)

let rec insert_sort xs = 
  match xs with
  | [] -> []
  | x::xs' -> 
      let ys' = insert_sort xs' in 
      insert ys' x

let insert_sort2 xs =
  List.fold_left insert [] xs

let rec check xs =
  match xs with
    | [] -> ()
    | x :: [] -> ()
    | x :: x' :: xs' ->
        let _ = show x' in
        assert (x <= x'); check (x' :: xs')

let test xs =
  check (insert_sort xs);
  check (insert_sort2 xs)
