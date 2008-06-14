(* version 1 *)

type 'a sparseArray =  ((int * 'a) list) array

let list_vec_mult vec xs =
  List.fold_left (fun sum (i,f) -> sum +. (f *. Array.get vec i)) 0.0 xs

let mat_vec_mult vec mat =
    let res = Array.make (Array.length mat) 0.0 in
    Array.iteri (fun i xs -> Array.set res i (list_vec_mult vec xs)) mat;
    res

(* version 2 *)

type 'a sparseArrayI = int * int * ((int * 'a) list) array

let list_vec_multI vec xs =
  let rec loop sum = function 
    match xs with
    | [] -> sum
    | (i,f)::xs -> loop (sum +. (f *. Array.get vec i)) xs in
  loop 0.0 xs

let mat_vec_multI mat vec =
  let (row, _, data) = mat in
  let res = Array.make row 0.0 in
  let rec loop i =
    if i >= row then () else
      let sum = list_vec_mult vec (Array.get data i) in
      let _   = Array.set res i sum in
      loop (i+1) in
  let _ = loop 0 in
  res
