type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let create x d l r =
    Node (x, d, l, r, 0)

let rec add x data t =
  match t with
      Empty ->
        Node(x, data, Empty, Empty, 1)
    | Node(v, d, l, r, h) ->
        if x = v then
          Node(x, data, l, r, h)
        else if x < v then
          create v d (add x data l) r
        else
          create v d l (add x data r)
