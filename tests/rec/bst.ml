let show = fun x -> x

type 'a tree = 
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec size t = 
  match t with
  | Empty -> 0
  | Node (_,l,r) -> 1 + size l + size r

let create () = 
  Empty

let rec add x' t = 
  match t with
  | Empty -> 
      Node (x',Empty,Empty)
  | Node(x, l, r) ->
      if x' <= x then Node(x, add x' l, r)
      else Node(x, l, add x' r)

let rec append k xs ys = 
  match xs with
  | []     -> ys
  | x::xs' -> x::(append k xs' ys)

let rec toList t =
  match t with
  | Empty -> []
  | Node (x,l,r) -> append x (toList l) (x::(toList r))

let rec make_tree n = 
  if n <= 0 then create () else
    let t = make_tree (n-1) in
    add n t

let _ = show size

let check n = 
  let t = make_tree n in
  let _ = assert (n = size t) in
  ()

