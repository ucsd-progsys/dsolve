let rec len = function
  | []    -> 0
  | x::xs -> 1 + (len xs)

let rec expand f xs = 
  match xs with
  | []      -> []
  | x::xs'  -> (f x) @ (expand f xs')

let rec insert t x = 
  let (key,value) = x in 
  match t with 
  | []          -> [(key,[value])]
  | (k,vs)::t'  -> if k = key 
                   then (k, value::vs)::t' 
                   else (k, vs)::(insert t' (key,value))

let group kvs = 
  List.fold_left insert [] kvs

let collapse f gs = 
  List.map 
    (fun x -> match x with | (k, []) -> assert (0 = 1); assert false
                           | (k, (v::vs)) -> (k, List.fold_left f v vs)) 
    gs

let map_reduce xs mapper reducer = 
  let kvs = expand mapper xs in
  let gs  = group kvs in
  let rs  = collapse reducer gs in
  rs
