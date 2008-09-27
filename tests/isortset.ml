let ss = Myset.of_list
let se = Myset.eq

let rec ins x = function 
  | []    -> [x]
  | y::ys -> if x < y then x::y::ys else y::(ins x ys)

let rec insert_sort = function
  | []    -> []
  | x::xs -> ins x (insert_sort xs)

let test xs = 
  let ys = insert_sort xs in
  assert (se (ss xs) (ss ys))
