type 'a rlist = 
  | Nil 
  | One of 'a
  | Even of 'a rlist * 'a rlist
  | Odd  of 'a * 'a rlist * 'a rlist

let rec cons x = function
  | Nil -> One x
  | One y ->  Even(One(x), One(y))
  | Even(l1, l2) -> Odd(x, l1, l2)
  | Odd(y, l1, l2) -> Even(cons x l1, cons y l2)

let rec uncons = function
  | One x -> (x, Nil)
  | Even(l1, l2) ->
    let (x, l1) = uncons l1 and (y, l2) = uncons l2 in begin
      match l1 with
        Nil -> (x, One y)
      | One _ -> (x, Odd(y, l1, l2))
      | Even _ -> (x, Odd(y, l1, l2))
      | Odd _ -> (x, Odd(y, l1, l2))
    end
  | Odd(x, l1, l2) -> (x, Even(l1, l2))

let head_safe l =
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

let rec length = function
  | Nil -> 0
  | One _ -> 1
  | Even (l1, _) -> 2 * (length l1)
  | Odd (_, l1, _) -> 2 * (length l1) + 1

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
