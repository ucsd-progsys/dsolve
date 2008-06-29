type 'a t =
    Empty
  | Node of 'a * int * 'a t * 'a t * int

let rec height t =
  match t with
  | Empty -> 0
  | Node (_,_,l,r,_) -> if height l > height r then 1 + height l else 1 + height r

let show x = x

let create x d l r =
  let hl = height l and hr = height r in
  let fat = Node(x, d, l, r, show (if hl >= hr then hl + 1 else hr + 1)) in
    show fat

let check x d =
  create x d Empty Empty
