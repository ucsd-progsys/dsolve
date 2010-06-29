(* DSOLVE -dontminemlq *)

let abs x = if x < 0 then (-x) else x 

let assign a i j = Junkarray2.set a i j 

let rec dotsPrint n = 
  if n = 0 then () else begin 
    print_string "."; 
    dotsPrint (n-1)
  end

let queenPrint queenArray sz =
  let rec aux row = 
    if row = sz then () else
      let n = Junkarray2.get queenArray row in 
      dotsPrint (n - 1); 
      dotsPrint (sz - n); 
      aux (row + 1) 
  in aux 0

let test queenArray j =
  let q2j = Junkarray2.get queenArray j  in
  let rec aux i =
    if i < j then
      let q2i = Junkarray2.get queenArray i in
      let absqdiff = abs (q2j - q2i) in
      if q2i = q2j then false else 
        if absqdiff = j - i then false else 
          aux (i + 1) 
    else true
  in aux 0  

let queen sz =
  let queenArray = Junkarray2.make sz 0 in
  let rec loop row =
    let next = (Junkarray2.get queenArray row) + 1 in
    if sz < next then begin 
      assign queenArray row 0; 
      if row = 0 then () else loop (row - 1) 
    end else begin 
      assign queenArray row next;
      if test queenArray row then begin 
        if row = sz then begin 
          queenPrint queenArray sz; 
          loop row 
        end else 
          loop (row + 1) 
      end else 
        loop row
    end  
  in loop 0

let driver = 
  let _ = Random.init 555 in
  let sz = Random.int 1000 in
  if sz > 0 then queen sz
