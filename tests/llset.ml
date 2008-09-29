let ss = Myset.eq

(*
type 'a list2 = Nil2 | Cons2 of 'a list * 'a list2

let rec of_list2 xss =
  match xss with
  | Nil2                -> Myset.empty
  | Cons2 (xs,xss')     -> Myset.cup (Myset.of_list xs) (of_list2 xss')

let rec append xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> x::(append xs' ys)

let rec flatten xss =
  match xss with
  | Nil2                -> []
  | Cons2(xs,xss')      -> append xs (flatten xss')
*)
(*
let rec of_list2 xss =
  match xss with
  | []          -> Myset.empty
  | xs::xss'    -> Myset.cup (Myset.of_list xs) (of_list2 xss')

let rec flatten xss =
  match xss with
  | []          -> []
  | xs::xss'    -> xs @ (flatten xss')
*)
(*
let rec reverse xs = 
  match xs with
  | []          -> []
  | x::xs'      -> append (reverse xs') [x]

let reverse2 xs = 
  let rec f ys zs = 
    match ys with
    | []        -> zs
    | y::ys'    -> f ys' (y::zs) in
  f xs []

let reverse3 xs = 
  let rec f (ys,zs) = 
    match ys with
    | []        -> zs
    | y::ys'    -> f (ys', (y::zs)) in
  f (xs,[])

*)

let rec mem x ys = 
  match ys with
  | []          -> false
  | y::ys'      -> if x = y then true else mem x ys'

let rec delete x ys = 
  match ys with
  | []          -> []
  | y::ys'      -> if x = y then delete x ys' else x::(delete x ys') 

(*
let compact xs = 
  let rec f ys zs =
    match zs with
    | []        -> ys
    | z::zs'    -> if mem z ys then f ys zs' else f (z::ys) zs' in
  reverse (f [] xs)

let rec sorted_compact xs =
  match xs with
  | []          -> []
  | [x]         -> [x]
  | x::x'::xs'  -> if x = x' then compact (x'::xs') else x::(compact (x'::xs'))
*)

