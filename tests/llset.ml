let ss = Myset.eq

let rec append xs ys =
  match xs with
  | []          -> ys
  | x::xs'      -> x::(append xs' ys)

type 'a list2 = Nil2 | Cons2 of 'a list * 'a list2

let rec of_list2 xss =
  match xss with
  | Nil2                -> Myset.empty
  | Cons2 (xs,xss')     -> Myset.cup (Myset.of_list xs) (of_list2 xss')

let rec flatten xss =
  match xss with
  | Nil2                -> []
  | Cons2(xs,xss')      -> append xs (flatten xss')

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

(* STRANGE ERROR // assert fails in liquid/pattern.ml
*)
let reverse3 xs = 
  let rec f x = 
    let (ys, zs) = x in
    match ys with
    | []        -> zs
    | y::ys'    -> f (ys', (y::zs)) in
  f (xs,[])



let rec mem x ys = 
  match ys with
  | []          -> false
  | y::ys'      -> if x = y then true else mem x ys'

let rec delete x ys = 
  match ys with
  | []          -> []
(*| y::ys'      -> if x = y then delete x ys' else x::(delete x ys')  (* BUG *) *)
  | y::ys'      -> if x = y then delete x ys' else y::(delete x ys')  (* OK  *)

let compact xs = 
  let rec f ys zs =
    match zs with
    | []        -> ys
(*  | z::zs'    -> if mem z xs then f ys zs' else f (z::ys) zs' in (* BUG *) *)
    | z::zs'    -> if mem z ys then f ys zs' else f (z::ys) zs' in (* OK  *) 
  reverse (f [] xs)

let rec sorted_compact xs =
  match xs with
  | []          -> []
  | [x]         -> [x]
  | x::x'::xs'  -> if x = x' then compact (x'::xs') else x::(compact (x'::xs'))

let rec diff xs ys = 
  match xs with
  | []          -> []
  | x::xs'      -> if mem x ys then diff xs' ys else x::(diff xs' ys)

let rec gt x ys = 
  match ys with
  | []          -> []
  | y::ys'      -> if x < y then  y::(gt x ys') else gt x ys'   (* OK  *)
