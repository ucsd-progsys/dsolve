type 'a t = V of 'a

let test a =
  V(a)

let t b =
  let s = test b in s

(*
let min a b =
  if a < b then a else b

let wrapmin x y =
  V(min x y)

let unwrapmin u v =
  let k = wrapmin u v in k
(*  match wrapmin u v with
    | V c ->
        assert (c < u)
*)
*)
