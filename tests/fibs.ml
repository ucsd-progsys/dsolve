let rec fibs n =
  if n = 0 then
    []
  else
    let p = fibs (n - 1) in
      if n <= 2 then
	1::p
      else
	match p with
            [] -> []
          | r::l ->
              match l with
                  [] -> []
                | s::m -> (r + s)::p

let rec fib1 n =
  if n <= 1 then 1 else (fib1 (n-1)) + (fib1 (n-2))

let fib2 n =
  let t = Hashtbl.create 17 in
  let rec f n =
    if Hashtbl.mem t n then Hashtbl.find t n else 
      let r  = if n <= 1 then 1 else  (f (n-1)) + (f (n-2)) in
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

