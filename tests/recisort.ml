let rec ins l x = match l with
  | [] -> x :: []
  | h :: xs ->
      if x < h then
        x :: h :: xs
      else
        h :: (ins xs x)

let insert_sort lst =
  List.fold_left ins [] lst
