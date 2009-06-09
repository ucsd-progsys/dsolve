type ('a, 'b) boolean = T of 'a | F of 'b

let rec len xs = 
  match xs with 
  | [] -> 0
  | x::xs' -> 1 + len xs'

let rec partition f xs = 
  match xs with
  | [] -> ([], [])
  | x::xs' -> 
      let (ts,fs) = partition f xs' in
      (match f x with | T x' -> (x'::ts, fs) 
                      | F x' -> (ts, x'::fs))

let rec append k xs ys = 
  match xs with
  | []     -> k::ys
  | x::xs' -> x::(append k xs' ys)

let rec quicksort xs = 
  match xs with
  | [] -> 
      []
  | x::xs' ->
      let (ls, rs) = partition (fun y -> if y < x then T y else F y) xs' in
      let ls'      = quicksort ls in
      let rs'      = quicksort rs in
      append x ls' rs'

let rec check xs =
  match xs with
    | []             -> ()
    | x :: []        -> ()
    | x :: x' :: xs' -> assert (x <= x'); 
                        check (x' :: xs')

let test xs =
  check (quicksort xs)
