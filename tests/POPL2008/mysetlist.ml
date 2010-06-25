let ss = Myset.of_list

let rec append xs ys =
  match xs with
  | []     -> 
      assert (Myset.eq (ss ys) (Myset.cup (ss ys) (ss xs)));
      assert (ss xs = Myset.empty); 
      ys
  | x::xs' -> x::(append xs' ys)

let rec rev x =
  match x with
  | []    -> []
  | x::xs -> append (rev xs) [x]
