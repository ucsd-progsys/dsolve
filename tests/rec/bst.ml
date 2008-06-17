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
  (*| Node(x,l,r) when x' <= x ->
      Node (x, add x' l, r) 
  | Node(x,l,r) when x' >  x -> 
      Node(x,l,add x' r)
  | Node(_, _, _) ->
      assert false*)
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

