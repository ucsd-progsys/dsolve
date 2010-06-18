let rec bcopy_aux src des i m =
  if i = m then 
    () 
  else begin
    Junkarray.set des i (Junkarray.get src i);
    bcopy_aux src des (i+1) m
  end

let bcopy src des =
  let sz = Junkarray.length src in
  bcopy_aux src des 0 3 

(********************* Driver **********************)

let _ = 
  let sz = read_int () in 
  if 0 < sz then
    let src = Junkarray.init sz (fun i -> i) in
    let dst = Junkarray.make sz 0 in
    bcopy src dst
