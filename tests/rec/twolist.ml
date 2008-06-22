type 'a tree =
    Nil
  | Node of 'a tree * 'a tree

let rec size x =
  match x with
    | Nil -> 1
    | Node (a, b) -> size a + size b

let x0 = Nil 
let x1 = Node (x0,x0)
let x2 = Node (x1,x1)
let x3 = Node (x2,x2)
