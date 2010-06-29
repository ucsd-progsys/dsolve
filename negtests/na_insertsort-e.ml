(* DSOLVE -dontminemlq *)

let swap a i j =
  let tmpj = Junkarray2.get a j in
  let tmpi = Junkarray2.get a i in 
  Junkarray2.set a i tmpj; 
  Junkarray2.set a j tmpi 
  
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
          let ij_minus = Junkarray2.get arr j_minus in 
          let ij = Junkarray2.get arr j in  
          if ij < ij_minus then begin
            swap arr j j_minus; 
            inner j_minus
          end else 
            outer i_plus  
      in inner i  
  in outer start_plus 

let sorted a =
  let len = Junkarray2.length a in
  let rec s v i =
    let v' = Junkarray2.get a i in
    if v' < v then 
      false 
    else if len = i + 1 then 
      true 
    else s v' (i+1)
  in if len < 2 then true else s (Junkarray2.get a 0) 1

let driver =
  let _  = Random.init 555 in
  let sz = (Random.int 100) + 5 in
  let a  = Junkarray2.make sz 0 in 
  let _  = insertSort a 0 (sz+1) in
  sorted a 
