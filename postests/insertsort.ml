(* DSOLVE -dontminemlq *)

let swap a i j =
  let tmpj = Array.get a j in
  let tmpi = Array.get a i in 
  Array.set a i tmpj; 
  Array.set a j tmpi 
  
let insertSort arr start n =
  let limit = start + n in
  let start_plus = start + 1 in
  let rec outer i  =
    let i_plus = i + 1 in
    if limit < i_plus then () else 
      let rec inner j =
        let j_minus = j - 1 in
        if j < start_plus then 
          outer i_plus 
        else 
          let ij_minus = Array.get arr j_minus in 
          let ij = Array.get arr j in  
          if ij < ij_minus then begin
            swap arr j j_minus; 
            inner j_minus
          end else 
            outer i_plus  
      in inner i  
  in outer start_plus 

let sorted a =
  let len = Array.length a in
  let rec s v i =
    let v' = Array.get a i in
    if v' < v then 
      false 
    else if len = i + 1 then 
      true 
    else s v' (i+1)
  in if len < 2 then true else s (Array.get a 0) 1

let driver =
  let _  = Random.init 555 in
  let sz = (Random.int 100) + 5 in
  let a  = Array.make sz 0 in 
  let _  = insertSort a 0 (sz - 1) in
  sorted a 
