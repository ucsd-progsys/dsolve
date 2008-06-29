type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let rec height t =
  match t with
  | Empty -> 0
  | Node (_,_,l,r,_) -> if height l > height r then (height l + 1) else (height r + 1)

let rec hz t =
  match t with
  | Empty -> 0
  | Node (_, _, _, _, h) -> h

let show x = x

let create x d l r =
  let hl = height l and hr = height r in
  let fat = Node(x, d, l, r, show (if hl >= hr then hl + 1 else hr + 1)) in
    show fat
      
let create x d =
  Node(x, d, Empty, Empty, d-1)

let foo x d l r =
  Node(x, d, l, r, d-1)

let check t x d l r =
  let _ = hz t in
  let _ = height t in
  let _ = foo x d l r in
    create x d
