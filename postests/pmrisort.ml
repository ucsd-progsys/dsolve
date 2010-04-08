let rec ins l x = match l with
  | [] -> x :: []
  | h :: xs ->
      if x < h then
        x :: h :: xs
      else
        h :: (ins xs x)

let rec rev xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> rev xs' (x::ys) 

let insert_sort lst =
  List.fold_left ins [] lst

let rec check l =
  match l with
    | [] -> ()
    | x :: [] -> ()
    | x :: y :: ys ->
        assert (x <= y); check (y :: ys)

let test l =
  let l'  = insert_sort l in
  let l'' = rev l' [] in
    check (insert_sort l)
