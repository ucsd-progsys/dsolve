let show x = x

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'

(**************************************************************************************)
(******************************* DML Merge Sort ***************************************)
(**************************************************************************************)

type 'a llist = Nil | Cons of 'a list * 'a llist

let rec llen xss = 
  match xss with
  | Nil -> show 0
  | Cons(xs,xss') -> show (len xs + llen xss')

let check xs =
  llen xs

let rec initlist xs =
  match xs with
  | [] -> show Nil 
  | x1::xs' -> 
      begin
        match xs' with
        | [] -> Cons([x1],Nil)
        | x2::xs'' -> (* bug in DML version! *)
            let y = if x1 < x2 then x1::x2::[] else x2::x1::[] in
            let ys = initlist xs'' in
            Cons(y,ys)
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
  | Nil  -> show [] 
  | Cons(xs, xs') -> 
      match xs' with
          Nil -> xs
        | Cons (xs1, xs2) -> mergeall (merge2 xss)

let mergesort2 xs =
  mergeall (initlist xs)

let check xs = 
  let _ = assert (len xs = len (mergesort2 xs)) in
  ()
