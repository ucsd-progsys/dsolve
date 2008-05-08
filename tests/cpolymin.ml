type 'a t = V of 'a

let min a b =
  if a < b then a else b

let wrapmin x y =
  V(min x y)

let unwrapmin u v =
  match wrapmin u v with
    | V c -> assert (c > u)
