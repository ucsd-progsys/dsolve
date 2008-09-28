type 'a list2 = Nil2 | Cons2 of 'a list * 'a list2

let rec of_list2 xss =
  match xss with
  | Nil2                -> Myset.empty
  | Cons2 (xs,xss')     -> Myset.cup (Myset.of_list xs) (of_list2 xss')

let rec append xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> x::(append xs' ys)

let rec flatten xss =
  match xss with
  | Nil2                -> []
  | Cons2(xs,xss')      -> append xs (flatten xss')

(*
let rec of_list2 xss =
  match xss with
  | []          -> Myset.empty
  | xs::xss'    -> Myset.cup (Myset.of_list xs) (of_list2 xss')

let rec flatten xss =
  match xss with
  | []          -> []
  | xs::xss'    -> xs @ (flatten xss')
*)
