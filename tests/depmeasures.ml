type a =
    Odd of int
  | Even of int

let par x =
  match x with
      Even a -> 2*a
    | Odd a -> 2*a + 1

let y = Even 1

let j = (fun x -> x) par y

let blah = assert (par y = 2)
