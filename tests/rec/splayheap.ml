(* Splay Heaps: Okasaki's "Purely Functional Data Structures" p.50 Fig. 5.5. *)

let myfail s = 
  print_string s; assert false

let show x = x

type 'a t = 
  | E 
  | T of 'a * 'a t * 'a t 

let empty = E

let isEmpty h = 
  match h with
  | E           -> true
  | T (_,_,_)   -> false

let rec partition pivot t = 
  match t with
  | E -> 
      (E, E)
  | T (x, a, b) ->
      if x <= pivot then
        match b with
        | E -> 
            (T(x, a, E), E)
        | T (y, b1, b2) ->
            (if y <= pivot then
              let (small, big)  = partition pivot b2 in
              let r1            = T (y, T (x, a, b1), small) in
              let r2            = big in
              (r1, r2)
            else
              let (small, big)  = partition pivot b1 in
              let r1            = T (x, a, small) in
              let r2            = T (y, big, b2)  in
              (r1, r2))
      else
        match a with
        | E ->
            (E, T(x,E,b))
        | T (y, a1, a2) ->
            if y <= pivot then
              let (small, big)  = partition pivot a2 in
              let r1            = T (y, a1, small) in
              let r2            = T (x, big, b) in
              (r1, r2)
            else
              let (small, big)  = partition pivot a1 in
              let r1            = small in
              let r2            = T (y, big, T (x, a2, b)) in
              (r1, r2)

let insert x t =
  let (a, b) = partition x t in
  T (x, a, b)

let rec merge t' t =
  match t' with
  | E -> 
      t
  | T (x, a, b) ->
      let (ta, tb) = partition x t in
      let a'       = merge ta a in
      let b'       = merge tb b in
      T (x, a', b') 

let rec findMin t = 
  match t with
  | E -> 
      myfail "empty"
  | T (x, a, b) -> 
      (match a with 
       | E -> x
       | _ -> findMin a)

let rec findMin2 t = 
  match t with
  | E -> 
      myfail "empty"
  | T (x, a, b) -> 
      (match a with 
       | E -> (x, T (x, E, b))
       | _ -> let (x', a') = findMin2 a in
              (x', T (x, a', b)))

let rec deleteMin t = 
  match t with
  | E ->
      myfail "empty"
  | T (x, a, c) ->
      (match a with 
       | E -> 
           c
       | T (x', a', b) ->
           (match a' with 
           | E -> T (x, b, c)
           | _ -> T (x', deleteMin a', T (x, b, c))))

let rec deleteMin2 t = 
  match t with
  | E -> assert false 
  | T (x, a, c) ->
      (match a with 
       | E -> 
           (x, c)
       | T (x', a', b) ->
           (match a' with 
           | E -> 
               (x', T (x, b, c))
           | _ -> 
               let (m, a'') = deleteMin2 a' in
               (m, T (x', a'', T (x, b, c)))))

let rec app x ys zs = 
  match ys with
  | [] -> x :: zs
  | y :: ys' -> y :: (app x ys' zs)

let rec to_list t = 
  match t with
  | E -> []
  | T (x, a, b) -> app x (to_list a) (to_list b)

let rec check_sorted xs =
  match xs with 
  | [] -> 
      ()
  | x :: xs' -> 
      List.iter (fun x' -> assert (x <= x')) xs';
      check_sorted xs'

let to_list2 t = 
  let xs = to_list t in
  let _  = check_sorted xs in
  xs

  (*
let rec deleteMin t = 
  match t with
  | E ->
      myfail "empty"
  | T (x, E, b) ->
      b
  | T (y, T (x, E, b), c) ->
      T (y, b, c)
  | T (y, T (x, a, b), c) ->
      T (x, deleteMin a, T (y, b, c))

let rec deleteMin2 t = 
  match t with
  | E ->
      myfail "empty"
  | T (x, E, b) ->
      (x, b)
  | T (y, T (x, E, b), c) ->
      (x, T (y, b, c))
  | T (y, T (x, a, b), c) ->
      let (r, a') = deleteMin2 a in
      (r, T (x, a', T (y, b, c)))

let rec findMin t = 
  match t with
  | E ->
      myfail "empty"
  | T (x, E, b) -> 
      x
  | T (x, a, b) -> 
      findMin a
 
let rec findMin2 t = 
  match t with
  | E -> 
      myfail "empty"
  | T (x, E, b) -> 
      (x, T (x, E, b))
  | T (x, a, b) ->
      let (x', a') = findMin2 a in
      (x', T (x, a', b))
*)

