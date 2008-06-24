type 'a tree =
    Nil
  | Node of 'a * 'a tree * 'a tree

let rec size x =
  match x with
    | Nil -> 1
    | Node (_, a, b) -> size a + size b

let cons x t =
  match t with
  | Nil -> Node (x, t, t)
  | Node (_, _, _) -> Node(x, t, t)

let rec makelist n =
  if n = 0 then Nil else
    let t = makelist (n-1) in
      cons n t

let check x =
  let x0 = Nil  in
  let x1 = Node (x, x0, x0) in
  let x2 = Node (x, x1, x1) in
  let x3 = Node (x, x2, x2) in
  let _ = makelist x in
    ()
