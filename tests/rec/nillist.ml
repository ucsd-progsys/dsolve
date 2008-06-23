type 'a llist =
    Nil | Cons of 'a list * 'a llist

let show x = x
let rec spin () = spin ()

let rec len x =
  match x with
      [] -> 0
    | x :: xs -> 1 + len xs

let rec llen x =
  match x with
      Nil -> 0
    | Cons(x1, xs) -> len x1 + llen xs


let check xs =
  llen xs

let check x =
  match x with
      Nil -> show Nil
    | Cons(x, xs) -> spin ()
