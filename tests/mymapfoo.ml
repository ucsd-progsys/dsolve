(* functions *)

let g x = x + 1

let m0 = g

let m1 = Mymap.set m0 2 3

let m2 = Mymap.set m1 3 4

let rec store n m =
  if n = 100 then m else
  let m' = Mymap.set m n (n+1) in
    store (n+1) m'

let mn = store 1 g

let rec retr n m =
  if n = 99 then () else
  let n' = Mymap.get m n in
  let _ = assert (n' > n) in
    retr (n+1) m

let _ = retr 1 mn

(* constructed types *)

let g' = Mymap.make 5 6

let m0 = Mymap.make 5 6 

let m1 = Mymap.sett m0 6 7

let m2 = Mymap.sett m1 8 9

let rec store n m =
  if n = 100 then m else
  let m' = Mymap.sett m n (n+1) in
    store (n+1) m'

let mn = store 1 g'

let rec retr n m =
  if n = 99 then () else
  let n' = Mymap.gett m n in
  let _ = assert (n' > n) in
    retr (n+1) m

let _ = retr 1 mn
