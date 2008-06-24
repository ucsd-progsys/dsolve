let show x = x

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'


(**************************************************************************************)
(******************************* Quick Sort *******************************************)
(**************************************************************************************)
(*
type ('a,'b) boolean = 
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

let rec quicksort xs = 
  match xs with
  | [] -> [] 
  | x::xs' ->
      let (ls,rs) = partition (fun y -> if y < x then True y else False y) xs' in
      let (ls',rs') = (quicksort ls, quicksort rs) in
      append x ls' (x::rs')

(**************************************************************************************)
(************************ Faster Quick Sort *******************************************)
(**************************************************************************************)

let reverse zs =
  let rec rev k rs xs = 
    match xs with
    | []    -> k::rs
    | y::ys -> rev y (k::rs) ys in
  match zs with
    | [] -> [] 
    | z::zs' -> rev z [] zs' 

let rec rev_append k xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> rev_append x xs' (x::ys)

let rec quicksort2 xs = 
  match xs with
  | [] -> []
  | x::xs' ->
      let (ls,rs) = partition (fun y -> if y < x then True y else False y) xs' in
      let (ls',rs') = (quicksort2 ls, quicksort2 rs) in
      rev_append x (reverse ls') (x::rs')

(**************************************************************************************)
(******************************* Merge Sort *******************************************)
(**************************************************************************************)
*)
let rec halve xs =
  match xs with
  | []   -> ([], [])
  | x::xs' ->
      let (ys, zs) = halve xs' in
      (x::zs, ys)


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


let rec mergesort ps = 
  match ps with
  | [] -> []
  | p::ps'  -> 
      let (qs,rs) = halve (show ps) in
      let qs' = mergesort (show qs) in
      let rs' = mergesort (show rs) in (* this is the problem! *)
      merge qs' rs'


let _ = show mergesort


(*

let splitter n =
  let rec f a b = 
    if read_int () > 0 
    then f (a-1) (b+1) 
    else (a,b) in
  f n 0

let joiner x y = 
  x + y

let tester n = 
  let (a,b) = splitter n in
  joiner a b

let _ = show tester
*)
(**************************************************************************************)
(******************************* DML Merge Sort ***************************************)
(**************************************************************************************)

type 'a llist = Nil | Cons of 'a list * 'a llist

let rec llen xss = 
  match xss with
  | Nil -> 0
  | Cons(xs,xss') -> len xs + llen xss'

let rec initlist xs =
  match xs with
  | [] -> Nil 
  | x1::xs' -> 
      begin
        match xs' with
        | [] -> Cons([x1],Nil)
        | x2::xs'' -> (* bug in DML version! *)
            let y = if x1 < x2 then x1::x2::[] else x2::x1::[] in
            let ys = initlist xs'' in
            Cons(y,ys)
      end

let _ = show llen
let _ = show initlist

(* 
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
  | [] -> []
  | [xs] -> [xs]
  | xs1::xs2::xss' -> (merge xs1 xs2)::(merge2 xss')

let rec mergeall xss = 
  match xss with 
  | []  -> [] 
  | [xs] -> xs
  | _  -> mergeall (merge2 xss)

let mergesort2 xs =
  mergeall (initlist xs)
*)

(**************************************************************************************)
(******************************* Insertion Sort ***************************************)
(**************************************************************************************)


 
let rec insert x ys =
  match ys with 
  | [] -> [x]
  | y::ys' -> 
      if x <= y 
      then (x::y::ys')
      else (y::(insert x ys'))

let rec insertsort xs =
  match xs with
  | [] -> []
  | x::xs' -> insert x (insertsort xs')

(*let _ = show halve
let _ = show merge 
let _ = show mergesort
let _ = show insert 
let _ = show insertsort
let _ = show quicksort 
let _ = show quicksort2*)
 
let check xs = 
  (*let _ = assert (len xs = len (quicksort xs)) in 
  let _ = assert (len xs = len (quicksort2 xs)) in*) 
  let _ = assert (len xs = len (insertsort xs)) in
  let _ = assert (len xs = len (mergesort xs)) in
  ()

