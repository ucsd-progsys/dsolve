let rec ins x ys = 
  match ys with 
  | []     -> [x]
  | y::ys' -> if x < y then x::y::ys' else y::(ins x ys')

let rec insert_sort xs =
  match xs with
  | []     -> []
  | x::xs' -> ins x (insert_sort xs')

 type ('a, 'b) boolean = 
  | True of 'a
  | False of 'b

let rec partition f xs = 
  match xs with
  | [] -> ([], [])
  | x::xs' -> 
      let (ts,fs) = partition f xs' in
      (match f x with 
       | True x' ->  (x'::ts,fs)
       | False x' -> (ts,x'::fs))

let rec append k xs ys = 
  match xs with
  | []     -> ys
  | x::xs' -> x::(append k xs' ys)

let rec quick_sort xs = match xs with
  | [] -> []
  | x::xs ->
      let (ls,rs) = partition (fun y -> if y < x then True y else False y) xs in
      let (ls',rs') = (quick_sort ls, quick_sort rs) in
      append x ls' (x::rs')

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

type 'a llist = Nil | Cons of 'a list * 'a llist

let rec set_of xss =
  match xss with
    | Nil -> Myset.empty
    | Cons(xs, xss') -> Myset.cup (Myset.of_list xs) (set_of xss')

let rec initlist xs =
  match xs with
    | [] -> Nil 
    | x1::xs' ->
        begin
          match xs' with
            | [] -> Cons([x1], Nil)
            | x2::xs'' ->
                let ys = initlist xs'' in
                  if x1 < x2 then Cons(x1::x2::[],ys) else Cons(x2::x1::[],ys)  
        end

let rec merge xs ys = 
  match xs with
    | [] -> ys
    | x::xs' ->
        begin
          match ys with
            | [] -> xs
            | y::ys' -> 
                if x < y 
                then x::(merge xs' (y::ys')) 
                else y::(merge (x::xs') ys')
        end

let rec merge2 xss = 
  match xss with 
    | Nil -> Nil 
    | Cons(xs1, xs') -> 
        match xs' with
            Nil -> xss
          | Cons(xs2, xss') -> Cons((merge xs1 xs2), (merge2 xss'))

let rec mergeall xss = 
  match xss with 
    | Nil  -> [] 
    | Cons(xs, xs') -> 
        match xs' with
            Nil -> xs
          | Cons (xs1, xs2) -> mergeall (merge2 xss)

let mergesort2 xs =
  let xs' = initlist xs in
    mergeall xs'
