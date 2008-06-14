type 'a tree = 
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec size = function
  | Empty -> 0
  | Node (_,l,r) -> 1 + size l + size r

let create () = 
  Empty

let rec add x' = function
  | Empty -> 
      Node (x,Empty,Empty)
  | Node(x,l,r) when x' <= x ->
      Node (x, add x' l, r) 
  | Node(x,l,r) when x' >  x -> 
      Node(x,l,add x' r)

let rec append k xs ys = 
  match xs with
  | []     -> ys
  | x::xs' -> x::(append k xs' ys)

let rec toList = function
  | Empty -> []
  | Node (x,l,r) -> append x (toList l) (x::(toList r))

