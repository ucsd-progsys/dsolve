type 'a tree =
    Nil
  | Node of 'a * 'a tree * 'a tree

let rec size x =
  match x with
    | Nil -> 1
    | Node (_, a, b) -> size a + size b

let check x =
  let x0 = Nil  in
  let x1 = Node (x, x0, x0) in
(*  let x2 = Node (x, x1, x1) in
  let x3 = Node (x, x2, x2) in *)
    ()
