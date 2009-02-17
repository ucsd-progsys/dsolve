type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec append xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> x::(append xs' ys)

let rec of_tree t = 
  match t with
  | Leaf        -> Myset.empty
  | Node (x,l,r)-> Myset.cup (Myset.sng x) (Myset.cup (of_tree l) (of_tree r))

let rec tree_of_list xs = 
  match xs with
  | []          -> Leaf 
  | x::xs'      -> Node (x, Leaf, tree_of_list xs')

let rec list_of_tree t =
  match t with
  | Leaf        -> []
  | Node (x,l,r)-> x::(append (list_of_tree l) (list_of_tree r))

let rec tree_flip t = 
  match t with
  | Leaf        -> Leaf 
  | Node (x,l,r)-> Node (x, tree_flip r, tree_flip l)
  
let _ = of_tree (tree_of_list [1;2;3]) 
