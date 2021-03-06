(* DSOLVE -dontgenmlq *)

(* merge sort, adapted from an example of Xi and Pfenning
 * http://www.cs.bu.edu/~hwxi/DML/DML/examples/mergesort2.dml
 *)

type 'a llist = Nil | Cons of 'a list * 'a llist

let rec len xs = 
  match xs with 
    | [] -> 0
    | x::xs' -> 1 + len xs'

let rec llen xss = 
  match xss with
    | Nil -> 0
    | Cons(xs,xss') -> len xs + llen xss'
    
let rec set_of xss =
  match xss with
    | Nil -> Myset.empty
    | Cons(xs, xss') -> Myset.cup (Myset.of_list xs) (set_of xss')

let check xs xss =
  let _ = set_of xss in
  llen xs

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
    
