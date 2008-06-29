let show x = x

type 'a rlist = 
  | Nil 
  | One of 'a
  | Even of 'a rlist * 'a rlist
  | Odd  of 'a * 'a rlist * 'a rlist

(*type rlist = 
  | Nil 
  | One of int
  | Even of rlist * rlist
  | Odd  of int * rlist * rlist*)

let rec sz l = 
  match l with
  | Nil -> 0
  | One x -> 1
  | Even (l1, l2) -> (sz l1) + (sz l2)
  | Odd (_, l1, l2) -> (1 + (sz l1) + (sz l2)) 

let nil l =
  match l with
  | Nil -> 1
  | One _ -> 0
  | Even (_, _) -> 0
  | Odd (_, _, _) -> 0

let rec cons x xs =
  match xs with
  | Nil -> (One x)
  | One y -> (Even(One(x), One(y)))
  | Even(l1, l2) -> (Odd(x, l1, l2))
  | Odd(y, l1, l2) -> (Even(cons x l1, cons y l2))

let rec makelist n =
  if n = 0 then Nil else
  let l = (makelist (n-1)) in
   cons n l 

let rec uncons xs =
  let _ = assert (nil xs = 0) in
  match xs with
  | Nil -> let _ = assert (1 = 0) in assert false
  | One x -> (x, Nil)
  | Even(l1, l2) ->
    let _ = assert (sz l1 = sz l2) in
    let (x, l1) = uncons l1 in
    let (y, l2) = uncons l2 in
    let _ = assert (sz l1 = sz l2) in
    begin
      match l1 with
        Nil -> show (x, One y)
      | One _ -> show (x, Odd(y, l1, l2))
      | Even _ -> show (x, Odd(y, l1, l2))
      | Odd _ -> show (x, Odd(y, l1, l2))
    end
  | Odd(x, l1, l2) -> show (x, Even(l1, l2))

(*let rec destroylist n xs =
  if n = 1 then xs else destroylist (n-1) (snd (uncons xs))*)

let head_safe l =
  let (x, _) = uncons l in x

let tail_safe l =
  let (x, l) = uncons l in l

let head l = 
  match l with
  | Nil -> assert false 
  | One _ -> head_safe l
  | Even _ ->  head_safe l
  | Odd _ ->  head_safe l

let tail l =
  match l with
  | Nil -> assert false 
  | One _ -> tail_safe l
  | Even _ ->  tail_safe l
  | Odd _ ->  tail_safe l

let rec lookup l i =
  match l with
  | Nil -> let _ = assert (1 = 0) in assert false
  | One x -> if i = 0 then x else let _ = assert (1 = 0) in assert false 
  | Even (l1, l2) ->
    if 2 * (i / 2) = i then lookup l1 (i / 2) else lookup l2 (i / 2) 
  | Odd(x, l1, l2) ->
    if i = 0 then x
     else if 2 * (i / 2) = i then lookup l2 ((i - 1) / 2) 
          else lookup l1 ((i - 1) / 2)

let rec print_rlist l =
  match l with
    Nil -> ()
  | One _ -> let (x, _) = uncons l in ()(*print_int x; print_newline ()*)
  | Even _ -> let (x, l) = uncons l in ()(*print_int x; print_string "; "; print_rlist l*)
  | Odd _ -> let (x, l) = uncons l in ()(*print_int x; print_string "; "; print_rlist l*)

let check x n n' i = 
  if n > 0 then
    let xs = makelist n in
    let _ = sz xs in
    let _ = nil xs in
    let _ = assert (n = sz xs) in
    let _ = show sz in (* BUG! we're losing the argument labels when we push sz through show *)
    let _ =
      let ys = uncons xs in
        assert(sz (snd ys) = sz xs - 1) in
    (* simpler test of head and tail *)
    let _ = head xs in
    let _ = tail xs in
    let _ = print_rlist xs in
    let _ = 
      if (i < n) && (i >= 0) then 
        let _ = lookup xs i in () else () in
  (*let _ =
    let ys = destroylist n xs in
      assert (nil ys = 1) in*)
      () else ()
