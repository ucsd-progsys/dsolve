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

let rec quicksort lst = match lst with
  | [] -> [] 
  | x::xs ->
      let (ls,rs) = partition (fun y -> if y > x then True y else False y) xs in
      let (ls',rs') = (quicksort ls, quicksort rs) in
      append x ls' (x::rs')

let rec check l =
  match l with
    | [] -> ()
    | x :: [] -> ()
    | x :: y :: ys ->
        assert (x <= y); check (y :: ys)

let test lst =
  check (quicksort lst)
