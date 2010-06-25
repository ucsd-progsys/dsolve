type nat = Z | S of nat

let rec fibs = function 
  | Z           -> 1
  | S Z         -> 1 
  | S S n       -> fibs n + fibs (S n)

let rec fib1 n =
  if n <= 1 then 1 else (fib1 (n-1)) + (fib1 (n-2))

(* n:{v <= 1} |- {v = 1} <: {v = fib1 n} 
   fib1: n:int -> { n <= 1 => v=1 }       *)

let fib2 n =
  let t = Hashtbl.create 17 in
  let rec f n =
    if Hashtbl.mem t n then Hashtbl.find t n else 
      let r  = if n <= 1 then fib1 n else (f (n-1)) + (f (n-2)) in
      let () = Hashtbl.add t n r in r in
  f n

let _ = fibs 10

let _ = fib1 10

let _ = fib2 10


let show x = ()

let make n f = 
  fun i -> if 0 <= i && i < n then ((f i) : int) else assert false 

let get a (i: int) = a i

let set a (i: int) v = fun j -> if i = j then v else a i 

let inc i = i+1
let a0 = make 1000 inc
let _ = a0
let _ = make

