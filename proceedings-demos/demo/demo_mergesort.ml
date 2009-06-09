let ss = S.elts

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'

let rec halve xs =
  match xs with
  | []   -> ([], [])
  | x::xs' ->
      let (ys, zs) = halve xs' in
      (x::zs, ys)

let rec merge xs ys =
  match xs, ys with
  | [],_           -> ys
  | _ ,[]          -> xs
  | x::xs', y::ys' -> if x < y 
                      then x::(merge xs' (y::ys')) 
                      else y::(merge (x::xs') ys')

let rec mergesort ps =
  match ps with 
  | []    -> []
  | [p]   -> [p]
  | _     -> let (qs, rs) = halve ps in
             let qs'      = mergesort qs in
             let rs'      = mergesort rs in
             merge qs' rs'


let rec check xs =
  match xs with
    | x :: x' :: xs' -> assert (x <= x'); check (x' :: xs')
    | _              -> ()


let test xs =
  check (mergesort xs)

