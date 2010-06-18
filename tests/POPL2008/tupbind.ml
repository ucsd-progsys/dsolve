(*
let foo k = (k-2, k-1)
let _     =
  let (p,q) = foo 10 in
  let _     = show (p,q) in 
  assert (p < q) 

let test1 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, i) else assert false in
  let (b,js) = show w in
  (fun j -> assert (b < j)) js

let test2 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then (a, [i]) else assert false in
  let (b,js) = show w in
  List.iter (fun j -> assert (b < j)) js

let test3 x =
  let w = 
    let a = read_int () in
    let i = read_int () in
    if a < i then show (a, [i]) else assert false in
  let (b,js) = w in
  List.iter (fun j -> assert (b < j)) js

*)

type foo = C of int

let test4 a i = 
  let ww = 
    if a < i then 
            let tt = (a, C i) in tt 
    else assert false in
  let (b,js) = ww in
  match js with C j -> assert (b < j)


(*let test5 a i =
  let w = 
    if a < i then let t = (a, C i) in show t 
    else assert false in
    (* if a < i then show (a, C i) else assert false in *)
  let (b,js) = w in
  match js with C j -> assert (b < j)
*)

let rec find n = function 
  | [] -> None 
  | (n':'a,v':'a list)::ns' -> if n = n' then Some v' else find n ns'

let new_edge g n n' =
  let ns = find n g in 
  (n,[n'])::g

let checkg g = 
  List.iter 
    (fun p -> 
       let (n,ns) = p in 
       List.iter (fun n' -> assert (n < n')) ns) g

let test6 xs = 
  let g0 = [] in
  let xs = [1;2;3;4] in
  let g  = List.fold_left (fun g m -> new_edge g m (m+1)) g0 xs in
  g

