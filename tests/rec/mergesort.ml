
let rec initlist = function
  | [] -> []
  | [x] -> [[x]]
  | x1::x2::xs -> (* bug in DML version! *)
      let y = if x1 < x2 then [x1;x2] else [x2;x1] in
      let ys = initlist xs in
      y::ys

let rec merge xs ys = 
  match xs, ys with
  | [],_  -> ys
  | _ ,[] -> xs
  | (x::xs'),(y::ys') ->
      if x < y 
      then x::(merge xs' (y::ys')) 
      else y::(merge (x::xs') ys')

let rec merge2 xss = 
  match xss with 
  | [] -> []
  | [xs] -> [xs]
  | xs1::xs2::xss' -> (merge xs1 xs2)::(merge2 xss')

let rec mergeall xss = 
  match xss with 
  | []  -> [] 
  | [xs] -> xs
  | _  -> mergeall (merge2 xss)

let sort xs =
  mergeall (initlist xs)
