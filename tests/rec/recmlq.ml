type 'a lst = Nil | Cons of 'a * 'a lst

let test (xs: 'a lst) = match xs with
  | Nil -> ()
  | Cons (x, xs) ->
      match xs with
        | Nil -> ()
        | Cons (y, _) -> assert (y > x)
