let rec bcopy_aux src des i m =
  if i = m then 
    () 
  else begin
    Junkarray2.set des i (Junkarray2.get src i);
    bcopy_aux src des (i+1) m
  end

let bcopy src des =
  let sz = Junkarray2.length src in
  bcopy_aux src des 0 sz

(********************* Driver **********************)

let _ = 
  let sz = read_int () in 
  if 0 < sz then
    let src = Junkarray2.init sz (fun i -> i) in
    let dst = Junkarray2.make sz 0 in
    bcopy src dst
