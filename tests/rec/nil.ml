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
  | Even (l1, l2) -> let x = sz l1 in let y = sz l2 in x + y
  | Odd (_, l1, l2) -> let x = 1 in let y = sz l1 in let z = sz l2 in let zz = y + z in x + zz 

let check xs = sz xs

let create () = Nil

let zz = Nil

let x = create ()

let y = show x

let z = show zz 
          
