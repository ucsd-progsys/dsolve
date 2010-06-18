let rec partition f xs = 
  match xs with 
  | []          -> ([],[])
  | x::xs'      -> let (ys,zs) = partition f xs' in
                   if f x then x::ys, zs else ys, x::zs

let rec append xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> x::(append xs' ys)
  
(************************* InsertSort *************************)

let rec ins x ys = 
  match ys with 
  | []     -> [x]
  | y::ys' -> if x < y then x::y::ys' else y::(ins x ys')

let rec insert_sort xs =
  match xs with
  | []     -> []
  | x::xs' -> ins x (insert_sort xs')

(************************* Quicksort **************************)

let rec quick_sort xs = 
  match xs with
  | []          -> []
  | x::xs'      -> let (gs,ls) = partition ((<) x) xs' in
                   append (quick_sort ls) (x::(quick_sort gs))

(************************* Mergesort **************************)

let rec halve xs =
  match xs with
  | []          -> ([], [])
  | x::xs'      -> let (ys, zs) = halve xs' in 
                   (x::zs, ys)

let rec merge xs ys =
  match xs with
  | []          -> ys
  | x::xs'      ->
      begin
        match ys with
        | []     -> xs
        | y::ys' -> 
            if x < y 
            then x::(merge xs' (y::ys')) 
            else y::(merge (x::xs') ys')
      end

let rec merge_sort xs =
  match xs with
  | []          -> []
  | x::xs'      -> 
      (match xs' with [] -> [x] | _ -> 
        let (ys, zs) = halve xs in
        let ys'      = merge_sort ys in
        let zs'      = merge_sort zs in
        merge ys' zs')

(*************************** Tests ****************************)
let ss = Myset.of_list
let test xs =
  let _ = let ys = insert_sort xs in 
          assert (Myset.eq (ss xs) (ss ys)) in
  let _ = let ys = quick_sort xs in
          assert (Myset.eq (ss xs) (ss ys)) in
  let _ = let ys = merge_sort xs in
          assert (Myset.eq (ss xs) (ss ys)) in
  ()
