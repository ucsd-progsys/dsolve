let y = Myset.mem

let rec append xs ys =
  match xs with
  | [] -> let _ = assert 
    (Myset.eq (Myset.of_list ys) (Myset.cup (Myset.of_list ys) (Myset.of_list xs))) in 
    let _ = assert (Myset.of_list xs = Myset.empty) in
    ys
  | x::xs' -> x::(append xs' ys)

let rec rev x =
  match x with
  | [] -> []
  | x::xs -> append (rev xs) [x]
