let show x = x

let rec spin () = spin ()

(*type 'a rlist = 
  | Nil 
  | One of 'a
  | Even of 'a rlist * 'a rlist
  | Odd  of 'a * 'a rlist * 'a rlist*)

type rlist = 
  | Nil 
  | One of int
  | Even of rlist * rlist
  | Odd  of int * rlist * rlist

let rec sz l = 
  match l with
  | Nil -> 0
  | One x -> 1
  | Even (l1, l2) -> (sz l1) + (sz l2)
  | Odd (_, l1, l2) -> (1 + (sz l1) + (sz l2)) 

let rec cons x xs =
  match xs with
  | Nil -> One x
  | One y ->  Even(One(x), One(y))
  | Even(l1, l2) -> Odd(x, l1, l2)
  | Odd(y, l1, l2) -> Even(cons x l1, cons y l2)

let create () = Nil
                        
let rec makelist n =
  if n = 0 then show (create ()) else (* unsoundness *)
  let l = show (makelist (n-1)) in
   cons n l 

let rec uncons xs =
  match xs with
  | Nil -> spin ()
  | One x -> show (x, Nil)
  | Even(l1, l2) ->
    let (x, l1) = uncons l1 in
    let (y, l2) = uncons l2 in
    begin
      match l1 with
        Nil -> show (x, One y)
      | One _ -> show (x, Odd(y, l1, l2))
      | Even _ -> show (x, Odd(y, l1, l2))
      | Odd _ -> show (x, Odd(y, l1, l2))
    end
  | Odd(x, l1, l2) -> show (x, Even(l1, l2))

let check xs x n = 
  let _ = sz xs in
  let _ = show sz in (* BUG! we're losing the argument labels when we push sz through show *)
  let _ = 
    let ys = cons x xs in
      assert(sz ys = 1 + sz xs) in
  let _ =
    let xs = makelist n in
    let _ = assert (n = sz xs) in
    let ys = uncons xs in
      assert(sz (snd ys) = sz xs - 1) in
    ()


(*let head_safe l =
  let (x, _) = uncons l in x

let tail_safe l =
  let (x, l) = uncons l in l

let head = function
  | Nil -> assert false 
  | One _ as l -> head_safe l
  | Even _ as l ->  head_safe l
  | Odd _ as l ->  head_safe l

let tail = function
  | Nil -> assert false 
  | One _ as l -> tail_safe l
  | Even _ as l ->  tail_safe l
  | Odd _ as l ->  tail_safe l

let rec lookup l i =
  match l with
  | Nil -> assert false
  | One x -> if i = 0 then x else assert false 
  | Even (l1, l2) ->
    if i mod 2 = 0 then lookup l1 (i / 2) else lookup l2 (i / 2) 
  | Odd(x, l1, l2) ->
    if i = 0 then x
     else if i mod 2 = 0 then lookup l2 ((i - 1) / 2) 
          else lookup l1 ((i - 1) / 2) 

let rec print_rlist = function
    Nil -> ()
  | One _ as l -> let (x, _) = uncons l in print_int x; print_newline ()
  | Even _ as l -> let (x, l) = uncons l in print_int x; print_string "; "; print_rlist l
  | Odd _ as l -> let (x, l) = uncons l in print_int x; print_string "; "; print_rlist l 
*)

